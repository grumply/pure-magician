module Pure.Magician.Server.Serve where

import Pure.Magician.Resources

import Pure.Auth (Username(..))
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

--------------------------------------------------------------------------------

serveAll :: forall a. (Server a, ServeMany a (Resources a)) => WebSocket -> Maybe Username -> IO ()
serveAll = serveMany @a @(Resources a)

class ServeMany (a :: *) (xs :: [*]) where
  serveMany :: WebSocket -> Maybe Username -> IO ()

instance ServeMany a '[] where
  serveMany socket mu = void do
    defaultServe @Admins socket mu 

instance (Server a, Servable a x (Elem x (Discussions a)), ServeMany a xs) => ServeMany a (x : xs) where 
  serveMany ws mun = serve @a @x @(Elem x (Discussions a)) ws mun >> serveMany @a @xs ws mun

class Servable (a :: *) (resource :: *) (discuss :: Bool) where
  serve :: WebSocket -> Maybe Username -> IO ()

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

-- Default instance for a uncached resource with discussion.
instance ( ServeConstraints resource True ) => Servable a resource True where
  serve = defaultServeWithDiscussion @resource

-- Default instance for a uncached resource without discussion.
instance ( ServeConstraints resource False ) => Servable a resource False where
  serve = defaultServe @resource

defaultServeWithDiscussion :: forall x.  ( ServeConstraints x True ) => WebSocket -> Maybe Username -> IO ()
defaultServeWithDiscussion ws = \case
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

defaultServe :: forall x.  ( ServeConstraints x False ) => WebSocket -> Maybe Username -> IO ()
defaultServe ws = \case
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

instance (Removable x (Elem x (Discussions a)), RemoveMany a xs) => RemoveMany a (x : xs) where
  removeMany ws = remove @x @(Elem x (Discussions a)) ws >> removeMany @a @xs ws

instance RemoveMany a '[] where
  removeMany = defaultRemove @Admins

-- This is required to be able to swap out authenticated and unauthenticated
-- enpoints when the user's token changes.
class Removable (resource :: *) (discussion :: Bool) where
  remove :: WebSocket -> IO ()

instance 
  ( Typeable resource
  , ToJSON (Context resource), FromJSON (Context resource)
  ) => Removable resource False 
  where
    remove = defaultRemove @resource 

instance 
  ( Typeable resource
  , ToJSON (Context resource), FromJSON (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource)
  ) => Removable resource True 
  where
    remove = defaultRemoveWithDiscussion @resource

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

