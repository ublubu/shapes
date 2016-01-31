module GameInit where

import Data.Text
import qualified SDL.Init as I
import qualified SDL.Hint as H
import qualified SDL.Video as V
import qualified SDL.Video.Renderer as R
import Foreign.C.Types
import Linear.V2

runMain :: String -> V2 CInt -> (R.Renderer -> IO ()) -> IO ()
runMain windowTitle screenSize main = do
  I.initialize [I.InitVideo, I.InitTimer, I.InitEvents]

  _ <- H.setHintWithPriority H.DefaultPriority H.HintRenderScaleQuality H.ScaleLinear
  window <- V.createWindow (pack windowTitle)
            (V.defaultWindow { V.windowInitialSize = screenSize })
  renderer <- V.createRenderer window (-1) R.defaultRenderer

  main renderer

  V.destroyRenderer renderer
  V.destroyWindow window
  I.quit
