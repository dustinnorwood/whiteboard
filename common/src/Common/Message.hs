{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Message where

import           Control.Lens
import           Data.Aeson (ToJSON, FromJSON, toEncoding, parseJSON,
                            defaultOptions, Options,
                            genericToEncoding, genericParseJSON)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)

data DrawEvent = DrawEvent
  { color :: T.Text
  , start :: Maybe (Int, Int)
  , end   :: (Int, Int)
  } deriving (Eq, Show, Generic)

data RoomConfig = RoomConfig
  { _roomName :: Text
  , _roomPassword :: Text
  } deriving (Eq, Show, Generic)
makeLenses ''RoomConfig

data Client a = Client
  { _clientName :: Text
  , _clientRoom :: Text
  , _clientConn :: a
  } deriving (Eq, Show, Functor, Generic)
makeLenses ''Client

data Room a = Room
  { _roomConfig :: RoomConfig
  , _roomClients :: Map Text (Client a)
  , _roomDrawing :: [DrawEvent]
  } deriving (Eq, Show, Functor, Generic)
makeLenses ''Room

newRoom :: RoomConfig -> Client a -> Room a
newRoom rc c = Room rc cm []
  where cm = M.singleton (_clientName c) c

data ServerState a = ServerState
  { _rooms :: Map Text (Room a)
  } deriving (Eq, Show, Functor, Generic)
makeLenses ''ServerState

newServerState :: ServerState a
newServerState = ServerState M.empty

numClients :: ServerState a -> Int
numClients = sum . map (length . _roomClients) . M.elems . _rooms

clientExists :: Client a -> ServerState a -> Bool
clientExists client ss =
  let c = _clientName client
      f = (== c)
      g = any f . M.keys . _roomClients
   in or . map g . M.elems $ _rooms ss

addClient :: Client a -> Text -> ServerState a -> ServerState a
addClient client r = rooms . at r . _Just . roomClients %~ f
  where f = M.insert (_clientName client) client

removeClient :: Client a -> ServerState a -> ServerState a
removeClient client ss =
  let m = M.singleton (_clientName client) client
      updateRoom = roomClients %~ (M.\\ m)
   in ServerState $ updateRoom <$> _rooms ss

data C2S = C2SCreateRoom Text RoomConfig
         | C2SJoinRoom Text RoomConfig
         | C2SDraw DrawEvent
         | C2SClear
         deriving (Eq, Show, Generic)

data S2C = S2Cwelcome Text
         | S2Cbroadcast Text
         | S2Cuserexists
         | S2Cnameproblem
         | S2CRoomDoesntExist RoomConfig
         | S2CRoomAlreadyExists Text
         | S2CJoinedRoom [DrawEvent]
         | S2CDraw DrawEvent
         | S2CClear
         deriving (Eq,Show, Generic)

options :: Options
options = defaultOptions -- { tagSingleConstructors = True }

instance ToJSON DrawEvent where toEncoding = genericToEncoding options
instance FromJSON DrawEvent where parseJSON = genericParseJSON options

instance ToJSON RoomConfig where toEncoding = genericToEncoding options
instance FromJSON RoomConfig where parseJSON = genericParseJSON options

instance ToJSON a => ToJSON (Client a) where toEncoding = genericToEncoding options
instance FromJSON a => FromJSON (Client a) where parseJSON = genericParseJSON options

instance ToJSON a => ToJSON (Room a) where toEncoding = genericToEncoding options
instance FromJSON a => FromJSON (Room a) where parseJSON = genericParseJSON options

instance ToJSON a => ToJSON (ServerState a) where toEncoding = genericToEncoding options
instance FromJSON a => FromJSON (ServerState a) where parseJSON = genericParseJSON options

instance ToJSON C2S where toEncoding = genericToEncoding options
instance FromJSON C2S where parseJSON = genericParseJSON options

instance ToJSON S2C where toEncoding = genericToEncoding options
instance FromJSON S2C where parseJSON = genericParseJSON options