module SDL.Event where

import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Event as Event
import Control.Monad.Extra
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Ptr
import Data.Maybe

data Key = Q | W | E | A | S | D | N

type Input = Maybe SDL.T.Event

pollQuit :: IO (Maybe SDL.T.Event) -> IO Bool
pollQuit stream = do
    maybeEvent <- stream
    case maybeEvent of
        Nothing -> return False

        Just (SDL.T.QuitEvent _ _) -> return True

        _ -> return False

pollEvent :: IO Input
pollEvent = alloca pollEvent_

pollEvent_ :: Ptr SDL.T.Event -> IO Input
pollEvent_ ptr = do
  status <- Event.pollEvent ptr

  if status == 1
    then do
    evt <- maybePeek peek ptr
    case evt of
--      Just (SDL.T.MouseMotionEvent { SDL.T.mouseMotionEventX = x
--                                   , SDL.T.mouseMotionEventY = y}) ->
--        print (x, y)
      _ -> return ()
    return evt
    else return Nothing

flushEvents :: IO [SDL.T.Event]
flushEvents = alloca (\ptr -> fmap catMaybes $ sequenceWhile isJust $ repeat (pollEvent_ ptr))

getKey :: SDL.T.Keysym -> Key
getKey sym = case SDL.T.keysymScancode sym of
  20 -> Q
  26 -> W
  8  -> E
  4  -> A
  22 -> S
  7  -> D
  _  -> N

