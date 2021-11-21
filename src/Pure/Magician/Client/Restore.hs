module Pure.Magician.Client.Restore where

import Control.Concurrent (forkIO)
import Pure.Elm.Application

restore = do
  forkIO do
    void do
      -- Should be sufficient for most devices?
      -- The failure mode is simply not restoring 
      -- the scroll position, which isn't too bad.
      delay (Millisecond * 100)
      addAnimation restoreScrollPosition