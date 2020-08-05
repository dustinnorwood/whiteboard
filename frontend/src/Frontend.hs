{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Lens
import Control.Monad
import Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Float (int2Double)
import Language.Javascript.JSaddle

import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core
import qualified Reflex.Dom.Canvas.Context2D as CanvasF
import qualified Reflex.Dom.CanvasBuilder.Types as Canvas
import qualified Reflex.Dom.CanvasDyn as Canvas
import Text.URI

import Common.Message
import Common.Route

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Whiteboard"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
    route <- getTextConfig "common/route"
    prerender_ blank $ app route
  }

app
  :: MonadWidget t m
  => Maybe Text
  -> m ()
app route = do
  rec
    msgEvDyn <- widgetHold loginWidget (ffor loggedInEv (whiteboardWidget clearOrDrawEv))
    let 
      msgSendEv = switch (current msgEvDyn)
      loggedInEv = fmapMaybe loginEv msgRecEv
      clearOrDrawEv = fmapMaybe isClearOrDrawEv msgRecEv
    msgRecEv <- wsEv route msgSendEv
  blank
  where
    loginEv = \case
      (S2CJoinedRoom rs) -> Just rs
      _ -> Nothing
    isClearOrDrawEv = \case
      (S2CDraw point) -> Just $ Right point
      S2CClear -> Just $ Left ()
      _ -> Nothing

loginWidget
  :: MonadWidget t m
  => m (Event t C2S)
loginWidget = el "div" $ do
  rec
    tUser <- el "div" . inputElement $ def
      & inputElementConfig_setValue .~ fmap (const "") eSubmit
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Enter Username")
    tName <- el "div" . inputElement $ def
      & inputElementConfig_setValue .~ fmap (const "") eSubmit
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Enter room name")
    tPass <- el "div" . inputElement $ def
      & inputElementConfig_setValue .~ fmap (const "") eSubmit
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Enter password")
    createRoom <- dynButton "Create room"
    joinRoom <- dynButton "Join room"
    -- Clean the name a bit (todo, clean more):
    let ev f = tag ((\a b c -> f a (RoomConfig b c)) <$> (fmap T.strip . current $ value tUser)
                      <*> (fmap T.strip . current $ value tName)
                      <*> (fmap T.strip . current $ value tPass))
        eCreate = ev C2SCreateRoom createRoom
        eJoin = ev C2SJoinRoom joinRoom
        eSubmit = leftmost [eCreate, eJoin]
  return eSubmit

whiteboardWidget
  :: MonadWidget t m
  => Event t (Either () DrawEvent)
  -> [DrawEvent]
  -> m (Event t C2S)
whiteboardWidget inDrawEv _ = do
  rec
    let canvasW = 1600 :: Int
        canvasH = 1200 :: Int
        canvasAttrs = M.fromList
          [ ("height" :: Text, T.pack (show canvasH))
          , ("width" , T.pack (show canvasW))
          ]

    -- Create the canvas element, using the backticked el function so Reflex.Dom provides us
    -- with the `El t`, which is the representation of the <canvas> element.
    drawEv <- elClass "div" "myCanvas" $ do
      (canvasEl, _ ) <- elAttr' "canvas" canvasAttrs blank

      -- Retrieve our CanvasInfo, restricted to 'context2d' because of the types! Yay! :D
      d2DDyn <- fmap (^. Canvas.canvasInfo_context) <$> Canvas.dContext2d ( Canvas.CanvasConfig canvasEl [] )
      let mdEv = domEvent Mousedown canvasEl
          mmEv = domEvent Mousemove canvasEl
          muEv = domEvent Mouseup canvasEl
          mxEv = domEvent Mouseleave canvasEl
      isMouseDown <- holdDyn False $ leftmost [True <$ mdEv, False <$ muEv, False <$ mxEv]
      let offset = (\(x,y) -> (x-7, y-7))
      let drawEv' = fmap (bimap offset offset) $ attachPromptlyDynWithMaybe (\a b -> if a then Just b else Nothing) isMouseDown (leftmost [Left <$> mdEv, Right <$> mmEv])
      lineCoords <- foldDyn (\ePoint (_,oldPoint) -> case ePoint of
                                                   Left p -> (Nothing, p)
                                                   Right p -> (Just oldPoint, p)) (Nothing, (0,0)) drawEv'
      let coordsEv = updated lineCoords
      let drawDyn ePoint = case ePoint of
            Left _ -> CanvasF.clearRectF 0.0 0.0 (fromIntegral canvasW) (fromIntegral canvasH)
            Right DrawEvent{..} -> do
              let ~(newX, newY) = end
                  (oldX, oldY) = case start of Just old -> old; Nothing -> (newX-3, newY-3)
              CanvasF.beginPathF
              CanvasF.strokeStyleF (textToJSString color)
              CanvasF.lineWidthF 5.0
              CanvasF.moveToF (int2Double oldX) (int2Double oldY)
              CanvasF.lineToF (int2Double newX) (int2Double newY)
              CanvasF.strokeF
              CanvasF.closePathF

      let myAction (d2D, ePoint) = do
            void . liftJSM $ CanvasF.drawToCanvas (drawDyn ePoint) d2D
      performEvent_ (ffor (attach (current d2DDyn) $ leftmost [fullDrawEv, inDrawEv]) myAction)
      return coordsEv
    (blackEv, blueEv, redEv, greenEv, whiteEv, clearEv) <- elClass "div" "button-bar" $ do
      bkEv <- fmap (const "#000000") <$> dynButton "Black"
      bEv <- fmap (const "#0000FF") <$> dynButton "Blue"
      rEv <- fmap (const "#FF0000") <$> dynButton "Red"
      gEv <- fmap (const "#00FF00") <$> dynButton "Green"
      wEv <- fmap (const "#FFFFFF") <$> dynButton "White"
      clEv <- dynButton "Clear"
      pure (bkEv, bEv, rEv, gEv, wEv, clEv)
    colorDyn <- holdDyn "#000000" $ leftmost [blackEv, blueEv, redEv, greenEv, whiteEv]
    let colorDrawEv = attachPromptlyDynWith (\c (s,e) -> DrawEvent c s e) colorDyn drawEv
        fullDrawEv = leftmost [Right <$> colorDrawEv, Left () <$ clearEv]
        outDrawEv = either (const C2SClear) C2SDraw <$> fullDrawEv
  pure outDrawEv

wsEv :: MonadWidget t m => Maybe Text -> Event t C2S -> m (Event t S2C)
wsEv route msgSendEv = case checkEncoder fullRouteEncoder of
  Left err -> do
    el "div" $ text err
    return never
  Right encoder -> do
    let wsPath = fst $ encode encoder $ FullRoute_Backend BackendRoute_Whiteboard :/ ()
        sendEv = fmap ((:[]) . toStrict . Aeson.encode) msgSendEv
    let mUri = do
          uri' <- mkURI =<< route
          pathPiece <- nonEmpty =<< mapM mkPathPiece wsPath
          wsScheme <- case uriScheme uri' of
            rtextScheme | rtextScheme == mkScheme "https" -> mkScheme "wss"
            rtextScheme | rtextScheme == mkScheme "http" -> mkScheme "ws"
            _ -> Nothing
          return $ uri'
            { uriPath = Just (False, pathPiece)
            , uriScheme = Just wsScheme
            }
    case mUri of
      Nothing -> return never
      Just uri -> do
        ws <- webSocket (render uri) $ def & webSocketConfig_send .~ sendEv
        let mS2c = fromStrict <$> _webSocket_recv ws
        pure $ fmapMaybe Aeson.decode mS2c 

dynButton :: MonadWidget t m => Text -> m (Event t ())
dynButton s = do
  (e, _) <- el' "button" $ text s
  pure $ domEvent Click e