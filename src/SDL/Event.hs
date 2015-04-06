module SDL.Event where

import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Event as Event
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable

handle :: IO (Maybe SDL.T.Event) -> IO Bool
handle stream = do
    maybeEvent <- stream
    case maybeEvent of
        Nothing -> return False

        Just (SDL.T.QuitEvent _ _) -> return True

        _ -> return False


pollEvent :: IO (Maybe SDL.T.Event)
pollEvent = alloca $ \pointer -> do
    status <- Event.pollEvent pointer

    if status == 1
        then maybePeek peek pointer
        else return Nothing
