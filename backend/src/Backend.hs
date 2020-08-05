{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Backend.Server (application)
import Common.Route
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Control.Concurrent
import qualified Network.WebSockets as WS
import Network.WebSockets.Snap
import Obelisk.Backend
import Common.Message

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      appState <- newMVar (newServerState @WS.Connection)
      serve $ \case
        BackendRoute_Missing :=> Identity () -> pure ()
        BackendRoute_Whiteboard :=> Identity () -> do
          runWebSocketsSnap $ application appState
  , _backend_routeEncoder = fullRouteEncoder
  }
