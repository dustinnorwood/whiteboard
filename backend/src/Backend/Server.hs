{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend.Server where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import           Control.Exception  (finally)
import           Control.Lens
import           Control.Monad      (forM_, forever, unless)
import           Data.Aeson         (encode, decode)
import qualified Data.ByteString    as B
import           Data.ByteString.Lazy (toStrict)
import qualified Data.Map.Strict    as M
import           Data.Semigroup     ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Network.WebSockets as WS

--------------------------------------------------------------------------------
import           Common.Message
--------------------------------------------------------------------------------

send :: WS.Connection -> S2C -> IO ()
send conn s2c = do
  T.putStrLn . T.pack $ show s2c
  WS.sendTextData conn . toStrict $ encode s2c

broadcast :: Text -> S2C -> ServerState WS.Connection -> IO ()
broadcast n s2c ss = do
  T.putStrLn . T.pack $ show s2c
  let clients = maybe M.empty _roomClients $ M.lookup n $ _rooms ss 
  forM_ clients $ \(Client _ _ conn) -> send conn s2c

broadcastExcept :: Text -> Text -> S2C -> ServerState WS.Connection -> IO ()
broadcastExcept n userName s2c ss = do
  T.putStrLn . T.pack $ show s2c
  let clients = maybe M.empty _roomClients $ M.lookup n $ _rooms ss 
  forM_ clients $ \(Client name _ conn) -> unless (name == userName) $ send conn s2c

broadcastAll :: S2C -> ServerState WS.Connection -> IO ()
broadcastAll s2c ss = do
  T.putStrLn . T.pack $ show s2c
  let clients = concatMap (M.elems . _roomClients) $ M.elems $ _rooms ss 
  forM_ clients $ \(Client _ _ conn) -> send conn s2c

application :: MVar (ServerState WS.Connection) -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  lobby state conn

lobby :: MVar (ServerState WS.Connection) -> WS.Connection -> IO ()
lobby state conn = do
  msgbs <- WS.receiveData conn :: IO B.ByteString
  let msgC = decode $ WS.toLazyByteString msgbs :: Maybe C2S
  case msgC of
      Nothing           ->
          T.putStrLn "Decoded msgC is nothing..."
      Just (C2SCreateRoom pName rc) -> do
        mState <- readMVar state
        let name = _roomName rc
        case M.lookup name $ _rooms mState of
          Just _ -> do
            send conn $ S2CRoomAlreadyExists name
            lobby state conn
          Nothing -> do
            let c = Client pName name conn
            modifyMVar_ state $ \s ->
              let r = newRoom rc c
                  f = rooms %~ M.insert name r
                  s' = f s
                in pure s'
            send conn (S2CJoinedRoom [])
            talk state c 
      Just (C2SJoinRoom pName rc) -> do
        mState <- readMVar state
        let name = _roomName rc
        case M.lookup name $ _rooms mState of
          Just r | _roomConfig r == rc -> do
            let c = Client pName name conn
            modifyMVar_ state $ \s ->
              let s' = addClient c name s
                in pure s'
            send conn (S2CJoinedRoom $ _roomDrawing r)
            talk state c
          _ -> do
            send conn $ S2CRoomDoesntExist rc
            lobby state conn
      Just msg -> do
        T.putStrLn $ "Got unexpected message: " <> T.pack (show msg)
        lobby state conn

talk :: MVar (ServerState WS.Connection) -> Client WS.Connection -> IO ()
talk state c@(Client user room conn) = flip finally disconnect . forever $ do
    msgbs <- WS.receiveData conn :: IO B.ByteString
    case decode $ WS.toLazyByteString msgbs of
        Nothing           ->
            T.putStrLn "Decoded msgC is nothing..."
        Just (C2SDraw point) -> readMVar state >>= broadcastExcept room user (S2CDraw point)
        Just C2SClear -> readMVar state >>= broadcastExcept room user S2CClear
        Just m -> T.putStrLn $ "Decoded " <> T.pack (show m)
  where
    disconnect = do
        -- Remove client and return new state
        s <- modifyMVar state $ \s ->
            let s' = removeClient c s in return (s', s')
        broadcast room (S2Cbroadcast $ user <> " disconnected") s