{-# language ImplicitParams #-}
module Pure.Magician.Client 
  ( client
  , useSocket
  , getAdmins, withAdmin, asAdmin, isAdmin
  , withRoute
  , unsafeWithRoute
  , getSomeRoute
  , RouteMany
  , WithRoute
  , Layout(..)
  , Restore(..)
  , Client(..)
  , Route(..)
  , App
  , module Export
  ) where

import Pure.Magician.Client.Restore as Export
import Pure.Magician.Resources as Export

import Pure.Auth (Username(..),Access(..),Token(..),authenticate,withToken,authorize,defaultOnRegistered)
import Pure.Conjurer as C hiding (Route,Routable)
import qualified Pure.Conjurer as C
import Pure.Convoker
import Pure.Data.JSON (ToJSON,FromJSON,traceJSON,logJSON)
import Pure.Data.Lifted (getPathname,getSearch)
import qualified Pure.Data.Txt as Txt
import qualified Pure.Data.View (Pure(..))
import qualified Pure.Elm
import Pure.Elm.Component (run,Component)
import Pure.Elm.Application as A hiding (layout)
import Pure.Hooks (provide,useContext)
import Pure.Router (dispatch,path,map,catchError,getRoutingState,putRoutingState,runRouting)
import qualified Pure.Router as R
import Pure.WebSocket
import Pure.WebSocket.Cache as WS
import Pure.Maybe

import Control.Applicative
import Control.Concurrent
import Data.Bool
import Data.Kind
import Data.List as List
import Data.Typeable
import System.IO
import System.IO.Unsafe

import Prelude hiding (map,not)

type RouteMany a = RouteMany' a (Domains a)
type WithRoute f a = WithRoute' f a (Domains a)

newtype Socket a = Socket WebSocket

useSocket :: forall a. Typeable (a :: *) => (WebSocket -> View) -> View
useSocket f = useContext (\((Socket ws) :: Pure.Magician.Client.Socket a) -> f ws)

client :: forall a domains. (Typeable a, Client a, RouteMany a, WithRoute (CRUL a) a, Application a, Restore a, Layout a) => String -> Int -> a -> IO ()
client host port a = do
  ws <- clientWS host port
  provide (Socket @a ws)
  hSetBuffering stdout LineBuffering
  -- TODO: make pure-websocket-cache amenable to role domains
  inject body (run (WS.Cache @a ws))
  inject body (execute (App ws a))

data App a = App WebSocket a

class Layout a where
  layout :: A.Route (App a) -> View -> View
  layout _ = id

instance {-# INCOHERENT #-} Layout a

class Restore a where
  restore :: IO ()
  restore = restoreWith (3 * Second) (100 * Millisecond)

instance {-# INCOHERENT #-} Restore a

instance (Typeable a, Client a, RouteMany a, WithRoute (CRUL a) a, Application a, Restore a, Layout a) => Application (App a) where
  data Model (App a) = AppModel (Model a)

  data Msg (App a) 
    = Startup
    | AppMsg (Msg a)

  data Route (App a) 
    = ClientR (SomeRoute a)  
    | AppR (A.Route a)

  initialize (App socket a) = do
    forkIO (void (authenticate @a socket))
    mdl <- initialize a
    pure (AppModel mdl)

  startup = [Startup] ++ fmap AppMsg startup
  
  receive = fmap AppMsg receive

  shutdown = fmap AppMsg shutdown 

  title (AppR r) = title r
  title _ = Nothing 

  location = \case
    ClientR (SomeRoute r) -> C.location r
    AppR r -> A.location r

  routes = routeMany @a @(Domains a) <|> R.map AppR (A.routes @a) 

  upon Startup _ (App socket _) mdl = do
    subscribeWith (AppMsg @a)
    pure mdl
  upon (AppMsg msg) (AppR r) (App _ a) (AppModel mdl) =
    AppModel <$> Pure.Elm.map (AppMsg @a) (upon msg r a mdl)
  upon (AppMsg msg) _ (App _ a) (AppModel mdl) = 
    AppModel <$> Pure.Elm.map (AppMsg @a) (upon msg home a mdl)
    
  route (ClientR _) _ _ mdl = do
    restore @a
    pure mdl
  route (AppR r) old (App _ a) m@(AppModel mdl) = do
    restore @a
    case old of
      AppR o -> do
        mdl' <- Pure.Elm.map (AppMsg @a) (A.route r o a mdl)
        pure (AppModel mdl')
      _ -> do
        mdl' <- Pure.Elm.map (AppMsg @a) (A.route r home a mdl)
        pure (AppModel mdl')

  home = AppR (home @a)

  view rt (App socket a) (AppModel mdl) =
    layout rt $
      case rt of
        ClientR sr -> 
          case withRoute @(CRUL a) sr (toPage @a) of
            Just v -> v
            _      -> Null

        AppR r -> 
          Pure.Elm.map (AppMsg @a) (view r a mdl)

class (Creatable a r, Readable r, Listable r, Updatable a r) => CRUL a r
instance (Creatable a r, Readable r, Listable r, Updatable a r) => CRUL a r

toPage :: forall a r. (Typeable a,Creatable a r,Readable r,Listable r,Updatable a r) => C.Route r -> View
toPage r = useSocket @a $ \ws -> pages @a ws r

class RouteMany' (a :: *) (as :: [*]) where
  routeMany :: Routing (A.Route (App a)) x

instance (Pure.Magician.Client.Routable a x, RouteMany' a xs) => RouteMany' a (x : xs) where
  routeMany = Pure.Magician.Client.route @a @x >> routeMany @a @xs

instance RouteMany' a '[] where
  routeMany = continue

class Routable a resource where
  route :: Routing (A.Route (App a)) ()

instance {-# OVERLAPPABLE #-}
  ( Typeable resource
  , Client a
  , Domains a ~ domains
  , Elem resource domains ~ True
  , C.Routable resource
  , FromJSON (Context resource), ToJSON (Context resource), Pathable (Context resource), Ord (Context resource)
  , FromJSON (Name resource), ToJSON (Name resource), Pathable (Name resource), Ord (Name resource)
  , Ownable resource
  ) => Pure.Magician.Client.Routable a resource 
    where
      route = C.routes @resource (ClientR . SomeRoute)

instance RouteMany a => Pathable (SomeRoute a) where
  toPath (SomeRoute r) = toPath r
  fromPath = do
    st <- getRoutingState
    (r,st') <- runRouting (routeMany @a @(Domains a)) st
    case r of
      Prelude.Left (Just (ClientR sr)) -> do
        putRoutingState st'
        pure (Just sr)
      _ -> do
        pure Nothing

withRoute :: forall constraint a x. (WithRoute' constraint a (Domains a)) => SomeRoute a -> (forall r. constraint r => C.Route r -> x) -> Maybe x
withRoute = withSomeRoute @constraint @a @(Domains a)

-- Unsafe if you have written a custom effectful `Routable` instance for one of the `Domains` of `a`. The
-- default derived instances are safe.
unsafeWithRoute :: forall a constraint x. (RouteMany a, WithRoute constraint a) => Txt -> (forall r. constraint r => C.Route r -> x) -> Maybe x
unsafeWithRoute t f 
  | Just (ClientR (sr :: SomeRoute a)) <- unsafePerformIO (R.route (routeMany @a @(Domains a)) t)
  = withRoute @constraint @a sr f

  | otherwise
  = Nothing

getSomeRoute :: forall domain. RouteMany domain => IO (Maybe (SomeRoute domain))
getSomeRoute = do
  pn <- getPathname 
  s  <- getSearch
  r <- R.route (routeMany @domain @(Domains domain)) (pn <> s)
  case r of
    Just (ClientR sr) -> pure (Just sr)
    _ -> pure Nothing

-- This approach should be okay up to a point, but it does incur overhead in testing
-- the TypeRep for a match against each element of rs. Hopefully the core for this is
-- reasonable and inlines all of this into a big case statement.
class WithRoute' (constraint :: * -> Constraint) (a :: *) (rs :: [*]) where
  withSomeRoute :: forall x. SomeRoute a -> (forall r. constraint r => C.Route r -> x) -> Maybe x

instance WithRoute' constraint a '[] where
  withSomeRoute _ _ = Nothing

instance (Typeable r, WithRoute' constraint a rs, constraint r) => WithRoute' constraint a (r : rs) where
  withSomeRoute sr@(SomeRoute x) f =
    case cast x of
      Just (r :: C.Route r) -> Just (f r)
      _                     -> withSomeRoute @constraint @a @rs sr f

getAdmins :: forall (a :: *). Typeable a => IO (Product (Admins a))
getAdmins =
  WS.req @a WS.Cached (readingAPI @(Admins a)) (readProduct @(Admins a)) (AdminsContext,AdminsName) >>= \case
    Nothing -> pure (Admins [])
    Just as -> pure as

withAdmin :: forall (a :: *). Typeable a => View -> View -> View
withAdmin notAdmin admin =
  withToken @a $ \case
    Just (Token (un,_)) -> 
      let 
        producer = do
          Admins as <- getAdmins @a
          pure (un `elem` as)
      in
        producing producer (consuming (bool notAdmin admin))
    Nothing -> 
      notAdmin

asAdmin :: forall (a :: *). Typeable a => View -> View
asAdmin = withAdmin @a Null

