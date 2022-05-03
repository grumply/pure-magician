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
  , Titled(..)
  , App
  , module Export
  , Policy(..)
  , Pure.Magician.Client.preview
  , Pure.Magician.Client.product
  , Pure.Magician.Client.listing
  , Top(..), Popular(..), Recent(..)
  , asUser
  , asOwner
  , asOwnerSomeRoute
  ) where

import Pure.Magician.Client.Restore as Export
import Pure.Magician.Resources as Export

import Pure.Async as Export hiding (eval)
import Pure.Async (eval)
import Pure.Auth (Username(..),Access(..),Token(..),authenticate,withToken,authorize,defaultOnRegistered)
import Pure.Conjurer as C hiding (Route,Routable)
import qualified Pure.Conjurer as C
import qualified Pure.Conjurer.Analytics as C
import Pure.Convoker
import Pure.Data.JSON (ToJSON,FromJSON,traceJSON,logJSON)
import Pure.Data.Lifted (getPathname,getSearch)
import qualified Pure.Data.Txt as Txt
import qualified Pure.Data.View (Pure(..))
import qualified Pure.Elm
import Pure.Elm.Fold (foldM)
import Pure.Elm.Has
import Pure.Elm.Component (run,Component)
import Pure.Elm.Application as A hiding (layout)
import Pure.Hooks (provide,useContext)
import Pure.Router (dispatch,path,map,catchError,getRoutingState,putRoutingState,runRouting)
import qualified Pure.Router as R
import Pure.WebSocket
import Pure.WebSocket.Cache as WS
import Pure.Maybe hiding (suspense)

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad hiding (foldM)
import Data.Bool
import Data.Kind
import Data.List as List
import Data.Maybe
import Data.Typeable
import System.IO
import System.IO.Unsafe
import System.Timeout

import Prelude hiding (map,not)

type RouteMany a = RouteMany' a (Domains a)
type WithRoute f a = WithRoute' f a (Domains a)

newtype Socket a = Socket WebSocket

useSocket :: forall a. Typeable (a :: *) => (WebSocket -> View) -> View
useSocket f = useContext (\((Socket ws) :: Pure.Magician.Client.Socket a) -> f ws)

client :: forall a domains. (Typeable a, Client a, RouteMany a, WithRoute Titled a, WithRoute (CRUL a) a, Application a, Restore a, Layout a) => String -> Int -> a -> IO ()
client host port a = do
  ws <- clientWS host port
  provide (Socket @a ws)
  hSetBuffering stdout LineBuffering
  inject body (run (WS.Cache @a ws))
  inject body (execute (App ws a))

data App a = App WebSocket a

class Layout a where
  layout :: A.Route (App a) -> View -> View
  layout _ = id

instance {-# OVERLAPPABLE #-} Layout a

class Restore a where
  restore :: IO ()
  restore = restoreWith (3 * Second) (100 * Millisecond)

instance {-# OVERLAPPABLE #-} Restore a

instance (Typeable a, Client a, RouteMany a, WithRoute Titled a, WithRoute (CRUL a) a, Application a, Restore a, Layout a) => Application (App a) where
  data Model (App a) = AppModel (Model a)

  data Msg (App a) 
    = Startup
    | AppMsg (Msg a)

  data Route (App a) 
    = ClientR (SomeRoute a)  
    | AppR (A.Route a)

  initialize (App socket a) = do
    forkIO do
      authenticate @a socket
      void do
        onStatus socket \case
          Opened -> void (authenticate @a socket)
          _ -> pure ()
    mdl <- initialize a
    pure (AppModel mdl)

  startup = [Startup] ++ fmap AppMsg startup
  
  receive = fmap AppMsg receive

  shutdown = fmap AppMsg shutdown 

  title (AppR r) = A.title r
  title (ClientR sr) = join (withRoute @Titled sr Pure.Magician.Client.title)

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

class Titled r where
  title :: C.Route r -> Maybe Txt

instance {-# OVERLAPPABLE #-} Titled r where
  title _ = Nothing

class (Creatable a r, Readable r, Listable r, Updatable a r) => CRUL a r
instance (Creatable a r, Readable r, Listable r, Updatable a r) => CRUL a r

toPage :: forall a r. (Typeable a,Creatable a r,Readable r,Listable r,Updatable a r) => C.Route r -> View
toPage r = useSocket @a $ \ws -> pages @a ws r

class RouteMany' (a :: *) (as :: [*]) where
  routeMany :: Routing (A.Route (App a)) x

instance (Pure.Magician.Client.Routable a x, RouteMany' a xs) => RouteMany' a (x : xs) where
  routeMany = Pure.Magician.Client.route @a @x >> routeMany @a @xs

instance RouteMany' a '[] where
  routeMany = R.continue

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

preview :: forall domain a. (Typeable domain, Elem a (Domains domain) ~ True, Conjurable a) => (Context a,Name a) -> Maybe (Preview a)
preview (!ctx,!nm) = eval (req @domain Cached (C.readingAPI @a) (C.readPreview @a) (ctx,nm))

product :: forall domain a. (Typeable domain, Elem a (Domains domain) ~ True, Conjurable a) => (Context a,Name a) -> Maybe (Product a)
product (!ctx,!nm) = eval (req @domain Cached (C.readingAPI @a) (C.readProduct @a) (ctx,nm))

listing :: forall domain a. (Typeable domain, Conjurable a, Elem a (Domains domain) ~ True) => Context a -> [(Name a,Preview a)]
listing !ctx = eval (fromMaybe [] <$> req @domain Cached (C.readingAPI @a) (C.readListing @a) ctx)

class Top domain a where
  tops :: a 

instance (Typeable domain, Conjurable a, Elem a (Analyze domain) ~ True) => Top domain [(Context a,Name a)] where
  tops = eval (req @domain Cached (C.analyticsAPI @a) (C.listTopForNamespace @a) ())

instance (Typeable domain, Conjurable a, Elem a (Analyze domain) ~ True) => Top domain (Context a -> [(Context a,Name a)]) where
  tops !ctx = eval (req @domain Cached (C.analyticsAPI @a) (C.listTopForContext @a) ctx)

instance (Typeable domain, Conjurable a, Elem a (Analyze domain) ~ True, RouteMany' domain (Analyze domain)) => Top domain ((Context a,Name a) -> [SomeRoute domain]) where
  tops (!ctx,!nm) = eval do
    rts <- req @domain Cached (C.analyticsAPI @a) (C.listRelatedTopForResource @a) (ctx,nm)
    catMaybes <$> traverse go rts
    where
      go rt = do
        x <- R.route (routeMany @domain @(Analyze domain)) rt
        case x of
          Just (ClientR sr) -> pure (Just sr)
          _ -> pure Nothing

class Popular domain a where
  populars :: a 

instance (Typeable domain, Conjurable a, Elem a (Analyze domain) ~ True) => Popular domain [(Context a,Name a)] where
  populars = eval (req @domain Cached (C.analyticsAPI @a) (C.listPopularForNamespace @a) ())

instance (Typeable domain, Conjurable a, Elem a (Analyze domain) ~ True) => Popular domain (Context a -> [(Context a,Name a)]) where
  populars !ctx = eval (req @domain Cached (C.analyticsAPI @a) (C.listPopularForContext @a) ctx)
  
instance (Typeable domain, Conjurable a, Elem a (Analyze domain) ~ True, RouteMany' domain (Analyze domain)) => Popular domain ((Context a,Name a) -> [SomeRoute domain]) where
  populars (!ctx,!nm) = eval do
    rts <- req @domain Cached (C.analyticsAPI @a) (C.listRelatedPopularForResource @a) (ctx,nm)
    catMaybes <$> traverse go rts
    where
      go rt = do
        x <- R.route (routeMany @domain @(Analyze domain)) rt
        case x of
          Just (ClientR sr) -> pure (Just sr)
          _ -> pure Nothing

class Recent domain a where
  recents :: a

instance (Typeable domain, Conjurable a, Elem a (Analyze domain) ~ True) => Recent domain [(Context a,Name a)] where
  recents = eval (req @domain Cached (C.analyticsAPI @a) (C.listRecentForNamespace @a) ())

instance (Typeable domain, Conjurable a, Elem a (Analyze domain) ~ True) => Recent domain (Context a -> [(Context a,Name a)]) where
  recents !ctx = eval (req @domain Cached (C.analyticsAPI @a) (C.listRecentForContext @a) ctx)

asUser :: forall (domain :: *). Typeable domain => View -> (Username -> View) -> View
asUser failed succeeded = withToken @domain (maybe failed (\(Token (un,_)) -> succeeded un))

asOwner :: forall (domain :: *) a. (Typeable domain, Eq (Context a), Eq (Name a), Ownable a) => Context a -> Maybe (Name a) -> View -> (Username -> View) -> View
asOwner ctx nm failed succeeded = 
  asUser @domain failed $ \un -> 
    let o = eval (isOwner un ctx nm)
    in bool failed (succeeded un) o

asOwnerSomeRoute :: forall domain. Typeable domain => SomeRoute domain -> View -> (Username -> View) -> View
asOwnerSomeRoute (SomeRoute sr) fallback success =
  case sr of
    ReadR ctx nm   -> asOwner @domain ctx (Just nm) fallback success
    UpdateR ctx nm -> asOwner @domain ctx (Just nm) fallback success
    CreateR ctx    -> asOwner @domain ctx Nothing   fallback success
    ListR ctx      -> asOwner @domain ctx Nothing   fallback success
