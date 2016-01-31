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

updater :: (a -> Word32 -> IO a) -> StateT a IO ()
updater f = do
  result <- get
  time <- SDL.Time.ticks
  result' <- liftIO $ f result time
  put result'
  return ()

timedUpdater :: Word32 -> (a -> Word32 -> IO a) -> StateT (Word32, a) IO ()
timedUpdater dt f = do
  (target, s) <- get
  time <- SDL.Time.ticks
  let wait = target - time in when (target > time) (liftIO $ threadDelay (1000 * fromIntegral wait))
  time' <- SDL.Time.ticks
  s' <- liftIO $ f s time'
  put (target + dt, s')
  return ()

timedRunWhile :: Word32 -> Word32 -> a -> (a -> Bool) -> (a -> Word32 -> IO a) -> IO ()
timedRunWhile t0 dt s0 test f = void $ runStateT u' (t0, s0)
  where u = timedUpdater dt f
        u' = do
          (_, s) <- get
          when (test s) $ u >> u'

runWhile :: a -> (a -> Bool) -> StateT a IO () -> IO ()
runWhile s0 test f = void $ runStateT f' s0
  where f' = do
          s <- get
          when (test s) $ f >> f'

runUntil :: a -> (a -> Bool) -> StateT a IO () -> IO ()
runUntil s0 test = runWhile s0 (not . test)

timedRunUntil :: Word32 -> Word32 -> a -> (a -> Bool) -> (a -> Word32 -> IO a) -> IO ()
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

