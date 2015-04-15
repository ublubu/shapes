module SDL.Event where

import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Event as Event
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable

data Key = Q | W | E | A | S | D | N

type Input = Maybe SDL.T.Event

handle :: IO (Maybe SDL.T.Event) -> IO Bool
handle stream = do
    maybeEvent <- stream
    case maybeEvent of
        Nothing -> return False

        Just (SDL.T.QuitEvent _ _) -> return True

        _ -> return False


pollEvent :: IO Input
pollEvent = alloca $ \pointer -> do
    status <- Event.pollEvent pointer

    if status == 1
        then maybePeek peek pointer
        else return Nothing

getKey :: SDL.T.Keysym -> Key
getKey sym = case SDL.T.keysymScancode sym of
  20 -> Q
  26 -> W
  8  -> E
  4  -> A
  22 -> S
  7  -> D
  _  -> N

