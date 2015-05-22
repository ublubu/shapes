module GameLoop where

import qualified Graphics.UI.SDL.Timer as SDL.Timer
import Control.Monad.State
import GHC.Word

updater :: (a -> Word32 -> IO a) -> StateT a IO ()
updater f = do
  result <- get
  time <- SDL.Timer.getTicks
  result' <- liftIO $ f result time
  put result'
  return ()

timedUpdater :: Word32 -> (a -> Word32 -> IO a) -> StateT (Word32, a) IO ()
timedUpdater t f = do
  (target, s) <- get
  time <- SDL.Timer.getTicks
  let wait = target - time in when (wait > 0) (liftIO $ SDL.Timer.delay wait)
  time' <- SDL.Timer.getTicks
  s' <- liftIO $ f s time'
  put (target + t, s')
  return ()

timedRunUntil :: Word32 -> Word32 -> a -> (a -> Bool) -> (a -> Word32 -> IO a) -> IO ()
timedRunUntil t0 dt s0 test f = runStateT u' (t0, s0) >> return ()
  where u = timedUpdater dt f
        u' = do
          (_, s) <- get
          if test s then u >> u'
          else return ()

runUntil :: a -> (a -> Bool) -> StateT a IO () -> IO ()
runUntil s0 test f = runStateT f' s0 >> return ()
  where f' = do
          s <- get
          if test s then f >> f'
          else return ()

