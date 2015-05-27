module SideScroller.GameState where

import qualified Graphics.UI.SDL.Types as SDL.T
import SideScroller.RectangularWorld
import SideScroller.Calculus
import Geometry

type GameBox = Box (Pair Double)
type GameBoxes = Boxes Double (Pair Double)

data Camera = Camera { cameraTransform :: Transform (Pair Double) }
data GameState = GameState { gameBoxes :: GameBoxes
                           , gameCamera :: Camera }

defaultGameState :: GameState
defaultGameState = GameState { gameBoxes = defaultBoxes
                             , gameCamera = defaultCamera }

defaultCamera :: Camera
defaultCamera = Camera { cameraTransform = Transform (Pair 40 40) (Pair 0 0) }

defaultBoxes :: GameBoxes
defaultBoxes = Boxes { boxes = [defaultBox]
                     , boxesTime = 0 }

defaultBox :: GameBox
defaultBox = gameBox 0 0 1 1 1 0

gameBox :: Double -> Double -> Double -> Double -> Double -> Double -> GameBox
gameBox x y w h x' y' =
  Box Transform { transformScale = BaseValue $ Pair w h
                , transformOrigin = BaseValue $ Pair x y } (toSpeed $ Pair x' y')

