{-# LANGUAGE PatternSynonyms #-}

module Physics.Test where

import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Enum as SDL.E
import GHC.Word
import Linear.Affine
import Linear.Matrix
import Linear.V2
import Physics.Linear
import Physics.TestWorld
import Physics.Transform
import Physics.Geometry
import Physics.Draw
import qualified SDL.Draw as D
import SDL.Event
import GameInit
import GameLoop
import Geometry
import Utils.Utils

main :: IO ()
main = runMain "physics test" (Pair 400 300) testMain
