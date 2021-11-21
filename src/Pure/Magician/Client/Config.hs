module Pure.Magician.Client.Config where

import Pure.Auth (Access(..),authorize,defaultOnRegistered)
import Pure.Conjurer (SomeRoute,goto,Route(..))
import Pure.Elm.Component (View)
import Pure.Maybe (producing)
import Pure.WebSocket (WebSocket)

{-
Unlike Pure.Magician.Server.Config, the client configuration
is dynamic and functional rather than static.
-}

data Config a = Config
  { host      :: String
  , port      :: Int
  , onStartup :: WebSocket -> IO ()
  , login     :: View
  , fallback  :: View
  , layout    :: SomeRoute a -> View -> View
  }
