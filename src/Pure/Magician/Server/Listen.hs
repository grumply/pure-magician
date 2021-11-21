module Pure.Magician.Server.Listen where
  
import Pure.Magician.Resources
import Pure.Magician.Server.Serve

import Pure.Sorcerer (Listener)
import Pure.Conjurer (Conjurable,conjure)
import Pure.Convoker (Convokable,convoke)

listenAll :: forall a. (Server a, ListenMany a (Resources a)) => [Listener]
listenAll = listenMany @a @(Resources a)

class ListenMany (a :: *) (xs :: [*]) where
  listenMany :: [Listener]

instance (Listenable a x (Elem x (Discussions a)), ListenMany a xs) => ListenMany a (x : xs) where
  listenMany = listen @a @x @(Elem x (Discussions a)) ++ listenMany @a @xs

instance ListenMany a '[] where
  listenMany = []

class Listenable (a :: *) (resource :: *) (discussion :: Bool) where
  listen :: [Listener]

-- Default listeners for resource without discussion.
instance {-# OVERLAPPABLE #-} (Conjurable resource) => Listenable a resource False where
  listen = conjure @resource

-- Default listeners for resource with discussion.
instance {-# OVERLAPPABLE #-} (Conjurable resource, Convokable resource) => Listenable a resource True where
  listen = conjure @resource ++ convoke @resource 

