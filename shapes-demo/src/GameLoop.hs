module GameLoop where

import qualified SDL.Time
import Control.Concurrent
import Control.Monad.State

import GHC.Word

testStep :: Integer -> Word32 -> IO Integer
testStep i t = do
  putStrLn $ "step " ++ show i ++ " at " ++ show t
  return (i + 1)

testTest :: Integer -> Bool
testTest = (> 5)

testTimedRunWhile :: IO ()
testTimedRunWhile = do
  t0 <- SDL.Time.ticks
  timedRunUntil t0 1000 0 testTest testStep

updater :: MonadIO m => (a -> Word32 -> IO a) -> StateT a m ()
updater f = do
  result <- get
  time <- SDL.Time.ticks
  result' <- liftIO $ f result time
  put result'
  return ()

timedUpdater :: MonadIO m
             => Word32
             -> (a -> Word32 -> m a)
             -> StateT (Word32, a) m ()
timedUpdater dt f = do
  (target, s) <- get
  time <- SDL.Time.ticks
  let wait = target - time in when (target > time) (liftIO $ threadDelay (1000 * fromIntegral wait))
  time' <- SDL.Time.ticks
  s' <- lift $ f s time'
  put (target + dt, s')
  return ()

timedRunWhile :: MonadIO m
              => Word32
              -> Word32
              -> a
              -> (a -> Bool)
              -> (a -> Word32 -> m a)
              -> m ()
timedRunWhile t0 dt s0 test f = void $ runStateT u' (t0, s0)
  where u = timedUpdater dt f
        u' = do
          (_, s) <- get
          when (test s) $ u >> u'

runWhile :: MonadIO m => a -> (a -> Bool) -> StateT a m () -> m ()
runWhile s0 test f = void $ runStateT f' s0
  where f' = do
          s <- get
          when (test s) $ f >> f'

runUntil :: MonadIO m => a -> (a -> Bool) -> StateT a m () -> m ()
runUntil s0 test = runWhile s0 (not . test)

timedRunUntil :: MonadIO m
              => Word32
              -> Word32
              -> a
              -> (a -> Bool)
              -> (a -> Word32 -> m a)
              -> m ()
timedRunUntil t0 dt s0 test = timedRunWhile t0 dt s0 (not . test)

timeSteps :: (RealFrac a, Integral b) => a -> a -> (b, a)
timeSteps dt tstep = (steps, remaining)
  where steps = floor (dt / tstep)
        remaining = max 0 (dt - fromIntegral steps * tstep)

timeSteps_ :: (Ord a, Num a, Integral b) => a -> a -> (b, a)
timeSteps_ dt = f (0, dt)
  where f res@(steps, remaining) tstep = if remaining > tstep
                                         then f (steps + 1, remaining - tstep) tstep
                                         else res

