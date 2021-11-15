{-# language AllowAmbiguousTypes, ScopedTypeVariables, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Pure.Magician.Client where

import qualified Pure.Conjurer as C
import Pure.Data.JSON (ToJSON(..),FromJSON(..))
import Pure.Elm.Application
import Pure.Elm.Component (Component)
import Pure.WebSocket
import Pure.WebSocket.Cache

import Control.Concurrent
import Data.Typeable
import System.IO

data Config = Config
  { host      :: String
  , port      :: Int
  , onStartup :: IO ()
  , fallback  :: WebSocket -> View
  , layout    :: WebSocket -> SomeRoute -> View -> View
  }

data SomeRoute 
  = forall resource. 
    ( Typeable resource
    , Theme resource
    , C.Routable resource
    , FromJSON (C.Resource resource), ToJSON (C.Resource resource), Default (C.Resource resource)
    , FromJSON (C.Context resource), ToJSON (C.Context resource), C.Pathable (C.Context resource), Eq (C.Context resource)
    , FromJSON (C.Name resource), ToJSON (C.Name resource), C.Pathable (C.Name resource), Eq (C.Name resource)
    , FromJSON (C.Preview resource)
    , FromJSON (C.Product resource)
    , C.Formable (C.Resource resource)
    , Component (C.Preview resource)
    , Component (C.Product resource)
    ) => SomeRoute (C.Route resource)

fromSomeRoute :: forall resource. Typeable resource => SomeRoute -> Maybe (C.Route resource)
fromSomeRoute (SomeRoute rt) = cast rt

class Client a where
  type Domains a :: [*]

client :: forall (a :: *) domains. (Typeable a, Client a, Domains a ~ domains, RouteMany a domains, Theme (App a)) => Config -> IO ()
client cfg@Config {..} = do
  ws <- clientWS host port
  hSetBuffering stdout LineBuffering
  inject body (cache ws)
  inject body (execute (App ws cfg :: App a))

data App (a :: *) = App WebSocket Config

instance (Domains a ~ domains, RouteMany a domains, Typeable a, Theme (App a)) => Application (App a) where
  data Msg (App a) = Startup

  startup = [Startup]

  upon Startup _ (App _ Config {..}) mdl = onStartup >> pure mdl

  data Route (App a) 
    = NoneR 
    | AppR SomeRoute
    
  route _new _old _app model = do
    forkIO do
      void do
        -- Should be sufficient for most devices?
        -- The failure mode is simply not restoring 
        -- the scroll position, which isn't too bad.
        delay (Millisecond * 100)
        addAnimation restoreScrollPosition
    pure model

  home = NoneR

  location = \case
    NoneR -> "/"
    AppR (SomeRoute sr) -> C.location sr

  routes = routeMany @a @(Domains a) (AppR . SomeRoute)

  view rt (App socket Config { fallback, layout }) _ =
    Div <| Themed @(App a) |>
      [ case rt of
          NoneR -> fallback socket
          AppR sr@(SomeRoute rt) -> layout socket sr (C.pages @a socket rt)
      ]

class RouteMany a as where
  routeMany :: 
    ( forall resource. 
      ( C.Routable resource 
      , Theme resource
      , FromJSON (C.Resource resource), ToJSON (C.Resource resource), Default (C.Resource resource)
      , FromJSON (C.Context resource), ToJSON (C.Context resource), C.Pathable (C.Context resource), Eq (C.Context resource)
      , FromJSON (C.Name resource), ToJSON (C.Name resource), C.Pathable (C.Name resource), Eq (C.Name resource)
      , FromJSON (C.Preview resource)
      , FromJSON (C.Product resource)
      , C.Formable (C.Resource resource)
      , Component (C.Preview resource)
      , Component (C.Product resource)
      ) => C.Route resource -> Route (App a)
    ) -> Routing (Route (App a)) x 

instance 
  ( Typeable a
  , Routable x
  , RouteMany a xs
  , Theme x
  , FromJSON (C.Resource x), ToJSON (C.Resource x), Default (C.Resource x)
  , FromJSON (C.Context x), ToJSON (C.Context x), C.Pathable (C.Context x), Eq (C.Context x)
  , FromJSON (C.Name x), ToJSON (C.Name x), C.Pathable (C.Name x), Eq (C.Name x)
  , FromJSON (C.Preview x)
  , FromJSON (C.Product x)
  , C.Formable (C.Resource x)
  , Component (C.Preview x)
  , Component (C.Product x)
  ) => RouteMany a (x : xs) where
  routeMany lift = do
    Pure.Magician.Client.route @x (AppR . SomeRoute)
    routeMany @a @xs lift

instance RouteMany a '[] where
  routeMany _ = dispatch NoneR

class Routable a where
  route :: (C.Route a -> route) -> Routing route ()

instance {-# INCOHERENT #-} (Typeable a, C.Routable a) => Routable a where
  route = C.routes