module Pure.Magician.Server.Serve where

import Pure.Magician.Resources

import Pure.Auth (Username(..))
import Pure.Conjurer.Analytics
import Pure.Conjurer as Conjurer
import Pure.Convoker as Convoker
import Pure.Convoker.UserVotes
import Pure.Elm.Component (Default)
import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.WebSocket (WebSocket, enact)
import qualified Pure.WebSocket as WS (remove)

import Control.Monad (void)
import Data.Kind
import Data.Typeable (Typeable)

{-

The goal here is to have a simple appoach to definining which resources are
cached, which resources have static pages generated for serving to crawlers,
and which resources have discussions associated with them. From these sets, a
server should be automatically generated that does all of the heavy lifting of 
setting up Pure.Sorcerer listeners, Pure.Conjurer caches, Pure.Conjurer static
pages, and Pure.Convoker discussion handling. 

Given these shared resources:

> -- Shared.hs
> data MyApp
>
> type instance Resources MyApp = [Resource1,Resource2,Resource3]
>
> {- resource definitions -}

A backend can look something like:

> -- Backend.hs
> main = serve @MyApp
>
> instance Serve MyApp where
>   type Caches MyApp = Resources MyApp \\ [Resource3] -- everything except Resource3
>   type Statics MyApp = Resources MyApp -- the default, omittable
>   type Discussions MyApp = [Resource1]

With this approach, given an average blog or simple website, a backend can be a
single line of code.

NOTE: If a resource is part of the Caches type family, both the resource and its
      associated discussion are cached. There is currently not way to cache a
      resource and not its discussion, or vice versa, without manually defining
      a Servable instance.

-}

class Server (a :: *) where
  type Caches a :: [*]
  type Caches a = Resources a \\ '[]

  type Statics a :: [*]
  type Statics a = Resources a \\ '[]

  type Discussions a :: [*]
  type Discussions a = '[]
  
  type Analyze a :: [*]
  type Analyze a = '[]

--------------------------------------------------------------------------------

serveAll :: forall a. (Server a, ServeMany a (Resources a)) => WebSocket -> SessionId -> Maybe Username -> IO ()
serveAll = serveMany @a @(Resources a)

class ServeMany (a :: *) (xs :: [*]) where
  serveMany :: WebSocket -> SessionId -> Maybe Username -> IO ()

instance ServeMany a '[] where
  serveMany socket sid mu = void do
    defaultServe @Admins socket sid mu 

instance (Server a, Servable a x (Elem x (Discussions a)) (Elem x (Analyze a)), ServeMany a xs) => ServeMany a (x : xs) where 
  serveMany ws sid mun = serve @a @x @(Elem x (Discussions a)) @(Elem x (Analyze a)) ws sid mun >> serveMany @a @xs ws sid mun

class Servable (a :: *) (resource :: *) (discuss :: Bool) (analyze :: Bool) where
  serve :: WebSocket -> SessionId -> Maybe Username -> IO ()

type family ServeConstraints a (discussion :: Bool) :: Constraint where
  ServeConstraints resource True = 
    ( ServeConstraints resource False
    , DefaultPermissions (Comment resource), DefaultPermissions (Meta resource)
    , DefaultCallbacks (Comment resource), DefaultCallbacks (Meta resource)
    , DefaultInteractions (Comment resource), DefaultInteractions (Meta resource)
    , DefaultCallbacks (Discussion resource), DefaultCallbacks (Mods resource), DefaultCallbacks (UserVotes resource)
    , Conjurable (Meta resource) 
    , Conjurable (Comment resource) 
    , Default (Resource (Meta resource))
    , Previewable (Meta resource)
    , Producible (Meta resource)
    , Producible (Comment resource)
    , Processable (Meta resource)
    , Processable (Comment resource)
    , Amendable (Meta resource)
    , Amendable (Comment resource)
    )
  ServeConstraints resource False = 
    ( Typeable resource
    , DefaultPermissions resource
    , DefaultCallbacks resource
    , DefaultInteractions resource
    , Conjurable resource
    , Previewable resource
    , Producible resource
    , Processable resource
    , Amendable resource
    )

-- Default instance for a uncached resource with discussion with analyze.
instance ( ServeConstraints resource True ) => Servable a resource True True where
  serve = defaultServeWithDiscussionWithAnalyze @resource

-- Default instance for a uncached resource with discussion without analyze.
instance ( ServeConstraints resource True ) => Servable a resource True False where
  serve = defaultServeWithDiscussion @resource

-- Default instance for a uncached resource without discussion with analyze.
instance ( ServeConstraints resource False ) => Servable a resource False True where
  serve = defaultServeWithAnalyze @resource

-- Default instance for a uncached resource without discussion without analyze.
instance ( ServeConstraints resource False ) => Servable a resource False False where
  serve = defaultServe @resource

defaultServeWithDiscussionWithAnalyze :: forall x.  ( ServeConstraints x True ) => WebSocket -> SessionId -> Maybe Username -> IO ()
defaultServeWithDiscussionWithAnalyze ws sid = \case
  Just un -> void do
    ip <- fromWebSocket ws
    enact ws (reading @x readPermissions (addAnalytics sid ip (callbacks (Just un))))
    enact ws (publishing @x (permissions (Just un)) (addAnalytics sid ip (addDiscussionCreationCallbacks [un] (callbacks (Just un)))) (interactions (Just un)))
    enact ws (analytics @x (permissions (Just un)))
    Convoker.authenticatedEndpoints @x ws un 
      (permissions (Just un))
      (permissions (Just un))
      (callbacks (Just un))
      (extendCommentCallbacks fullPermissions (callbacks (Just un)) (callbacks (Just un)))
      (callbacks (Just un))
      (callbacks (Just un))
      (callbacks (Just un))
      (interactions (Just un))
      (interactions (Just un))

  _ -> void do
    ip <- fromWebSocket ws
    enact ws (reading @x readPermissions (addAnalytics sid ip (callbacks Nothing)))
    enact ws (analytics @x (permissions Nothing))
    Convoker.unauthenticatedEndpoints @x ws
      (callbacks Nothing)
      (callbacks Nothing)
      (callbacks Nothing)

defaultServeWithDiscussion :: forall x.  ( ServeConstraints x True ) => WebSocket -> SessionId -> Maybe Username -> IO ()
defaultServeWithDiscussion ws _ = \case
  Just un -> void do
    enact ws (reading @x readPermissions (callbacks (Just un)))
    enact ws (publishing @x (permissions (Just un)) (addDiscussionCreationCallbacks [un] (callbacks (Just un))) (interactions (Just un)))
    Convoker.authenticatedEndpoints @x ws un 
      (permissions (Just un))
      (permissions (Just un))
      (callbacks (Just un))
      (extendCommentCallbacks fullPermissions (callbacks (Just un)) (callbacks (Just un)))
      (callbacks (Just un))
      (callbacks (Just un))
      (callbacks (Just un))
      (interactions (Just un))
      (interactions (Just un))

  _ -> void do
    enact ws (reading @x readPermissions (callbacks Nothing))
    Convoker.unauthenticatedEndpoints @x ws
      (callbacks Nothing)
      (callbacks Nothing)
      (callbacks Nothing)


defaultServeWithAnalyze :: forall x.  ( ServeConstraints x False ) => WebSocket -> SessionId -> Maybe Username -> IO ()
defaultServeWithAnalyze ws sid = \case
  Just un -> void do
    ip <- fromWebSocket ws
    enact ws (reading @x readPermissions (addAnalytics sid ip (callbacks (Just un))))
    enact ws (publishing @x (permissions (Just un)) (addAnalytics sid ip (callbacks (Just un))) (interactions (Just un)))
    enact ws (analytics @x (permissions (Just un)))

  _ -> void do
    ip <- fromWebSocket ws
    enact ws (analytics @x (permissions Nothing))
    enact ws (reading @x readPermissions (addAnalytics sid ip (callbacks Nothing)))

defaultServe :: forall x.  ( ServeConstraints x False ) => WebSocket -> SessionId -> Maybe Username -> IO ()
defaultServe ws _ = \case
  Just un -> void do
    enact ws (reading @x readPermissions (callbacks (Just un)))
    enact ws (publishing @x (permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))

  _ -> void do
    enact ws (reading @x readPermissions (callbacks Nothing))

--------------------------------------------------------------------------------

removeAll :: forall a. (Server a, RemoveMany a (Resources a)) => WebSocket -> IO ()
removeAll = removeMany @a @(Resources a)

class RemoveMany a (xs :: [*]) where
  removeMany :: WebSocket -> IO ()

instance (Removable x (Elem x (Discussions a)) (Elem x (Analyze a)), RemoveMany a xs) => RemoveMany a (x : xs) where
  removeMany ws = remove @x @(Elem x (Discussions a)) @(Elem x (Analyze a)) ws >> removeMany @a @xs ws

instance RemoveMany a '[] where
  removeMany = defaultRemove @Admins

-- This is required to be able to swap out authenticated and unauthenticated
-- enpoints when the user's token changes.
class Removable (resource :: *) (discussion :: Bool) (analyze :: Bool) where
  remove :: WebSocket -> IO ()

instance 
  ( Typeable resource
  , ToJSON (Context resource), FromJSON (Context resource)
  ) => Removable resource False False
  where
    remove = defaultRemove @resource 

instance 
  ( Typeable resource
  , ToJSON (Context resource), FromJSON (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource)
  ) => Removable resource True False
  where
    remove = defaultRemoveWithDiscussion @resource

instance 
  ( Typeable resource
  , ToJSON (Context resource), FromJSON (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource)
  ) => Removable resource False True
  where
    remove ws = do
      defaultRemove @resource ws
      defaultRemoveAnalytics @resource ws

instance 
  ( Typeable resource
  , ToJSON (Context resource), FromJSON (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource)
  ) => Removable resource True True
  where
    remove ws = do
      defaultRemoveWithDiscussion @resource ws
      defaultRemoveAnalytics @resource ws

defaultRemove :: forall (x :: *).  ( Typeable x, ToJSON (Context x), FromJSON (Context x) ) => WebSocket -> IO ()
defaultRemove ws = do
  WS.remove ws (readingAPI @x) 
  WS.remove ws (publishingAPI @x)

defaultRemoveWithDiscussion :: forall (x :: *).  ( Typeable x, ToJSON (Context x), FromJSON (Context x), ToJSON (Name x), FromJSON (Name x) ) => WebSocket -> IO ()
defaultRemoveWithDiscussion ws = do
  WS.remove ws (readingAPI @x)
  WS.remove ws (publishingAPI @x)
  WS.remove ws (readingAPI @(Discussion x))
  WS.remove ws (readingAPI @(Comment x))
  WS.remove ws (readingAPI @(Meta x))
  WS.remove ws (readingAPI @(Mods x))
  WS.remove ws (readingAPI @(UserVotes x))
  WS.remove ws (publishingAPI @(Comment x))
  WS.remove ws (publishingAPI @(Meta x))
  WS.remove ws (publishingAPI @(Mods x))
  WS.remove ws (publishingAPI @(UserVotes x))

defaultRemoveAnalytics :: forall (x :: *). (Typeable x) => WebSocket -> IO ()
defaultRemoveAnalytics ws = do
  WS.remove ws (analyticsAPI @x)