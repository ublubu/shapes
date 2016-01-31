module EasySDL.Draw where

import Data.StateVar
import qualified SDL.Video.Renderer as R
import GHC.Word
import Linear.V4

white :: V4 Word8
white = V4 0xFF 0xFF 0xFF 0xFF

red :: V4 Word8
red = V4 0xFF 0x00 0x00 0xFF

green :: V4 Word8
green = V4 0x00 0xFF 0x00 0xFF

blue :: V4 Word8
blue = V4 0x00 0x00 0xFF 0xFF

yellow :: V4 Word8
yellow = V4 0xFF 0xFF 0x00 0xFF

black :: V4 Word8
black = V4 0x00 0x00 0x00 0xFF

pink :: V4 Word8
pink = V4 0xFF 0x3E 0x96 0xFF

clearScreen :: R.Renderer -> IO ()
clearScreen renderer = do
  setColor renderer white
  R.clear renderer

setColor :: R.Renderer -> V4 Word8 -> IO ()
setColor r c = R.rendererDrawColor r $= c

withBlankScreen :: R.Renderer -> IO () -> IO ()
withBlankScreen r operation = do
  clearScreen r
  operation
  R.present r
