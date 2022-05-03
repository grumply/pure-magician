module Pure.Magician (module Export) where

import Pure.Async as Export hiding (eval)
import Pure.Auth as Export (Username,Token(..))
import Pure.Data.JSON as Export hiding (Index,Null)
import Pure.Data.Marker as Export hiding (hex)
import Pure.Data.Time as Export
import Pure.Data.Txt.Interpolate as Export
import Pure.Data.View 
import Pure.Elm.Fold as Export hiding (App,layout)
import Pure.Conjurer as Export hiding (root,publishing,List,Route,preview,previews)
import Pure.Magician.Client as Export
import Pure.Random.PCG as Export hiding (list,int)

import Control.Concurrent as Export hiding (yield)
import Control.Monad as Export hiding (foldM)
import Data.Bifunctor as Export
import Data.Either as Export
import Data.Foldable as Export (for_,traverse_)
import Data.Function as Export (fix,(&),on)
import Data.Maybe as Export
import Data.Traversable as Export (for,traverse)
import Data.Typeable as Export (Typeable,Proxy(..))
import Debug.Trace as Export
import GHC.Generics as Export (Generic)
import System.IO as Export
import System.Timeout as Export
import Text.Printf as Export (printf)
import Text.Read as Export (readMaybe)

-- | A variant of `fix` 'fixed' to an arrow (`->`) type with a convenient
-- ordering for inline usage: `flip fix`. For introducing local anonymous
-- recursive functions.
-- 
-- > loop initialState ( \k currentState -> ... k newState ... )
--
-- NOTE: the type of `loop`, while fixed to an arrow type, is still polymorphic
--       and remains covariant in the result type and can be used similarly
--       to the polymorphic `fix` to feed in extra parameters. That is: 
--
--       > loop2 :: x -> y -> ((x -> y -> a) -> x -> y -> a) -> a
--       > loop2 x = flip (loop x)
--
--       encodes the 2-variable recursive least fixed point of `a` and would be
--       used similarly to `loop` for introducing local anonymous recursive 
--       functions of two variables:
--
--       > loop2 x0 y0 ( \k x y -> ... k x1 y1 ... )
--
loop :: state -> ((state -> a) -> state -> a) -> a
loop = flip fix
