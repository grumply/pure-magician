module Pure.Magician.Server.Static where

import Pure.Magician.Resources
import Pure.Magician.Server.Serve

import Pure.Data.View (Pure)
import Pure.Conjurer
import Pure.Elm.Component (Component)
import Pure.WebSocket (WebSocket)

import Data.Typeable (Typeable)

staticAll :: forall a. (Server a, Subset (Statics a) (Resources a) ~ True, StaticMany a (Statics a)) => WebSocket -> IO ()
staticAll = staticMany @a @(Statics a)

class StaticMany a (xs :: [*]) where
  staticMany :: WebSocket -> IO ()

instance (Staticable a x (Elem x (Statics a)), StaticMany a xs) => StaticMany a (x : xs) where
  staticMany ws = static @a @x @(Elem x (Statics a)) ws >> staticMany @a @xs ws

instance StaticMany a '[] where
  staticMany _ = pure ()

class Staticable (a :: *) (resource :: *) (static :: Bool) where
  static :: WebSocket -> IO ()

instance {-# OVERLAPPABLE #-} (Conjurable resource, Routable resource, Pure (Product resource)) => Staticable a resource True where
  static = generateStatic @resource

instance Staticable a resource False where
  static _ = pure ()
