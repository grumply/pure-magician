module Pure.Magician.Client (module Pure.Magician.Client, Config(..), module Export) where

import Pure.Magician.Client.Config
import Pure.Magician.Client.Restore
import Pure.Magician.Resources as Export

import Pure.Auth (Access(..),Token(..),authenticate,withToken,authorize,defaultOnRegistered)
import Pure.Conjurer as C hiding (Route,Routable)
import qualified Pure.Conjurer as C
import Pure.Convoker
import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Elm.Application
import Pure.Hooks (provide,useContext)
import Pure.Router (dispatch,path)
import qualified Pure.Router as R
import Pure.WebSocket
import qualified Pure.WebSocket.Cache as WS
import Pure.Maybe

import Control.Concurrent
import Control.Monad
import Data.Bool
import Data.Typeable
import System.IO

newtype Socket a = Socket WebSocket

useSocket :: forall a. Typeable (a :: *) => (WebSocket -> View) -> View
useSocket f = useContext (\((Socket ws) :: Pure.Magician.Client.Socket a) -> f ws)

client :: forall a domains. (Typeable a, Client a, Domains a ~ domains, RouteMany a domains, Theme (App a)) => Config a -> IO ()
client cfg@Config {..} = do
  ws <- clientWS host port
  provide (Socket @a ws)
  hSetBuffering stdout LineBuffering
  -- TODO: make pure-websocket-cache amenable to role domains
  inject body (WS.cache ws)
  inject body (execute (App ws cfg :: App a))

data App a = App WebSocket (Config a)

instance {-# INCOHERENT #-} Typeable a => Theme (App a)

instance (Typeable a, Client a, Domains a ~ domains, RouteMany a domains, Theme (App a)) => Application (App a) where
  data Msg (App a) = Startup

  initialize (App socket Config {..}) = forkIO (void (authenticate @a socket)) >> pure undefined

  startup = [Startup]

  upon Startup _ (App socket Config {..}) mdl = onStartup socket >> pure mdl

  data Route (App a) 
    = NoneR
    | LoginR
    | ClientR (SomeRoute a)  
    
  route _ _ _ mdl = restore >> pure mdl

  home = NoneR

  location = \case
    NoneR -> "/"
    LoginR -> "/login"
    ClientR (SomeRoute r) -> C.location r

  routes = routeAll @a

  view rt (App socket Config { fallback, login, layout }) _ =
    Div <| Themed @(App a) |>
      [ case rt of
          NoneR -> fallback
          LoginR -> login
          ClientR sr@(SomeRoute rt) -> layout sr (pages @a socket rt)
      ]

instance Typeable a => Default (Config a) where
  def = Config "127.0.0.1" 8081 def 
    (useSocket @a $ \socket -> authorize @a (Access socket id defaultOnRegistered) (\(Token (un,_)) -> producing (R.goto "/") (\_ -> Null)))
    def
    (\_ v -> v)

class Client (a :: *) where
  type Domains a :: [*]
  type Domains a = Resources a

routeAll :: forall a x. (Client a, RouteMany a (Domains a)) => Routing (Route (App a)) x
routeAll = do
  path "/login" (dispatch LoginR)
  routeMany @a @(Domains a)

class RouteMany (a :: *) (as :: [*]) where
  routeMany :: Routing (Route (App a)) x

instance (Routable a x, RouteMany a xs) => RouteMany a (x : xs) where
  routeMany = Pure.Magician.Client.route @a @x >> routeMany @a @xs

instance RouteMany a '[] where
  routeMany = dispatch NoneR

class Routable a resource where
  route :: Routing (Route (App a)) ()

instance {-# OVERLAPPABLE #-}
  ( Typeable resource
  , Client a
  , Domains a ~ domains
  , Elem resource domains ~ True
  , Theme resource
  , C.Routable resource
  , FromJSON (Resource resource), ToJSON (Resource resource), Default (Resource resource)
  , FromJSON (Context resource), ToJSON (Context resource), Pathable (Context resource), Eq (Context resource)
  , FromJSON (Name resource), ToJSON (Name resource), Pathable (Name resource), Eq (Name resource)
  , FromJSON (Preview resource)
  , FromJSON (Product resource)
  , Formable (Resource resource)
  , Readable resource
  , Updatable a resource
  , Listable resource
  , Creatable a resource
  ) => Routable a resource 
    where
      route = C.routes @resource (ClientR . SomeRoute)

getAdmins :: IO (Product Admins)
getAdmins =
  WS.req WS.Cached (readingAPI @Admins) (readProduct @Admins) (AdminsContext,AdminsName) >>= \case
    Nothing -> pure (Admins [])
    Just as -> pure as

withAdmin :: forall (a :: *). Typeable a => View -> View -> View
withAdmin notAdmin admin =
  withToken @a $ \case
    Just (Token (un,_)) -> 
      let 
        producer = do
          Admins as <- getAdmins 
          pure (un `elem` as)
      in
        producing producer (consuming (bool notAdmin admin))
    Nothing -> 
      notAdmin

asAdmin :: forall (a :: *). Typeable a => View -> View
asAdmin = withAdmin @a Null
