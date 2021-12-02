module Pure.Magician.Server.Analytics where

import Pure.Magician.Resources
import Pure.Magician.Server.Serve

import Pure.Conjurer hiding (cache)
import qualified Pure.Conjurer as Conjurer
import qualified Pure.Conjurer.Analytics as Conjurer
import Pure.Convoker as Convoker
import Pure.Elm.Component
import Pure.Data.JSON (ToJSON,FromJSON)

import Control.Concurrent
import Control.Monad
import Data.Typeable

import Data.Hashable

analyzeAll :: forall a. (Server a, Subset (Analyze a) (Resources a) ~ True, AnalyzeMany a (Analyze a)) => Time -> IO ()
analyzeAll = analyzeMany @a @(Analyze a)

class AnalyzeMany a (xs :: [*]) where
  analyzeMany :: Time -> IO ()

instance AnalyzeMany a '[] where
  analyzeMany _ = pure ()

instance (Analyzable a x, AnalyzeMany a xs) => AnalyzeMany a (x : xs) where
  analyzeMany t = analyze @a @x t >> analyzeMany @a @xs t

class Analyzable a x where
  analyze :: Time -> IO ()

instance {-# OVERLAPPABLE #-}
  ( Typeable x
  , Routable x
  , Hashable (Context x), Pathable (Context x), Ord (Context x), ToJSON (Context x)
  , Hashable (Name x), Pathable (Name x), Ord (Name x), ToJSON (Name x)
  ) => Analyzable a x 
  where
    analyze t =
      void do
        forkIO do
          forever do
            Conjurer.analyze @x
            delay t
