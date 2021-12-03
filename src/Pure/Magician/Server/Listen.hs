module Pure.Magician.Server.Listen where
  
import Pure.Magician.Resources
import Pure.Magician.Server.Serve

import Pure.Auth (authDB)
import Pure.Sorcerer (sorcerer)
import Pure.Conjurer (Conjurable,conjure)
import Pure.Conjurer.Analytics
import Pure.Convoker (Admins,Convokable,convoke)

import Data.Typeable

listenAll :: forall a. (Server a, ListenMany a (Resources a)) => IO ()
listenAll = listenMany @a @(Resources a)

class ListenMany (a :: *) (xs :: [*]) where
  listenMany :: IO ()

instance (Listenable a x (Elem x (Discussions a)) (Elem x (Analyze a)), ListenMany a xs) => ListenMany a (x : xs) where
  listenMany = listen @a @x @(Elem x (Discussions a)) @(Elem x (Analyze a)) >> listenMany @a @xs

instance Typeable a => ListenMany a '[] where
  listenMany = do
    authDB @a 
    conjure @Admins 
    sorcerer @SessionMsg @'[Session]
    sorcerer @SessionsMsg @'[Sessions]

class Listenable (a :: *) (resource :: *) (discussion :: Bool) (analyze :: Bool) where
  listen :: IO ()

-- Default listeners for resource without discussion, not analyzed.
instance {-# OVERLAPPABLE #-} (Conjurable resource) => Listenable a resource False False where
  listen = conjure @resource

-- Default listeners for resource without discussion, analyzed.
instance {-# OVERLAPPABLE #-} (Conjurable resource) => Listenable a resource False True where
  listen = do
    sorcerer @(GlobalAnalyticsMsg resource) @'[GlobalAnalytics] 
    sorcerer @(ContextAnalyticsMsg resource) @'[ContextAnalytics] 
    sorcerer @(ResourceAnalyticsMsg resource) @'[ResourceAnalytics] 
    conjure @resource

-- Default listeners for resource with discussion, not analyzed.
instance {-# OVERLAPPABLE #-} (Conjurable resource, Convokable resource) => Listenable a resource True False where
  listen = do
    conjure @resource 
    convoke @resource 

-- Default listeners for resource with discussion, analyzed.
instance {-# OVERLAPPABLE #-} (Conjurable resource, Convokable resource) => Listenable a resource True True where
  listen = do
    sorcerer @(GlobalAnalyticsMsg resource) @'[GlobalAnalytics] 
    sorcerer @(ContextAnalyticsMsg resource) @'[ContextAnalytics] 
    sorcerer @(ResourceAnalyticsMsg resource) @'[ResourceAnalytics] 
    conjure @resource 
    convoke @resource 