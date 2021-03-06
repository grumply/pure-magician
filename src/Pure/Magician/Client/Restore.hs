{-# language CPP #-}
module Pure.Magician.Client.Restore where

import Pure.Data.Lifted (clientHeight,clientWidth,onRaw,toNode,getWindow)
import Pure.Elm.Application

import Control.Concurrent

{-
This method is far from perfect, but is designed to work with most off-the-shelf
`pure-magician`-based applications.
-}

restoreWith :: Time -> Time -> IO ()
restoreWith max d = 
  void do
    forkIO do
      mv <- newEmptyMVar
      w <- getWindow
      release <- onRaw (toNode w) "popstate" def $ \_ _ -> void (tryPutMVar mv ())
      go release mv
  where
    go release mv = loop max
      where
        loop remaining 

            -- browser never loaded enough content to correctly restore
          | remaining <= 0 = release

          | otherwise = do
            mu <- tryTakeMVar mv
            case mu of

              -- route change; fail out
              Just _  -> release 

              _ -> do
                -- Should be sufficient for most devices?
                -- The failure mode is simply not restoring 
                -- the scroll position, which isn't too bad.
                -- Sadly, there's no way to know if all the 
                -- desired content has loaded, which is the
                -- reason for the delay.
                withScrollPositionFromHistory $ \ox oy -> do
                  ch <- clientHeight
                  cw <- clientWidth
                  sh <- scrollHeight
                  sw <- scrollWidth
                  if sw >= ox + cw && sh >= oy + ch then do
                    addAnimation restoreScrollPosition
                    release
                  else do
                    delay d
                    loop (remaining - d)

scrollHeight :: IO Int
scrollHeight =
#ifdef __GHCJS__
  scroll_height_js
#else
  pure 0
#endif

scrollWidth :: IO Int
scrollWidth =
#ifdef __GHCJS__
  scroll_width_js
#else
  pure 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = document.documentElement.scrollHeight" 
    scroll_height_js :: IO Int

foreign import javascript unsafe
  "$r = document.documentElement.scrollWidth"
    scroll_width_js :: IO Int
#endif
