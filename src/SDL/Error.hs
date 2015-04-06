module SDL.Error where

import qualified Graphics.UI.SDL.Basic as SDL.B
import Foreign.C.String

type Risky a = Either String a


catchRisky :: Risky a -> IO a
catchRisky = either throwSDLError return


logWarning :: Risky Bool -> IO Bool
logWarning = either (\x -> print x >> return False) return


throwSDLError :: String -> IO a
throwSDLError message = do
    errorString <- SDL.B.getError >>= peekCString
    fail (message ++ " SDL_Error: " ++ errorString)
