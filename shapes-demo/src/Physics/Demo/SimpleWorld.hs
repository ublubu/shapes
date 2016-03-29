{-# LANGUAGE TypeFamilies #-}

module Physics.Demo.SimpleWorld where

import Control.Lens
import Control.Monad
import qualified Data.IntMap.Strict as IM

import qualified Physics.Broadphase.Simple.Aabb as B
import Physics.Contact.Simple
import Physics.Engine.Simple
import qualified Physics.Engine.Simple.Main as SM
import Physics.World.Simple
import Physics.Solvers.Simple (toShowableSolverState)

import Physics.Draw.Canonical
import qualified Physics.Draw.Simple as D
import Physics.Demo.World (Demo(..))

import Utils.Utils

instance Demo SimpleEngine where
  type DEngineState SimpleEngine = SM.EngineState
  initialEngineState _ = SM.defaultInitialState
  drawWorld _ = D.drawWorld
  demoWorld _ = _spFst
  worldContacts _ world =
    join $ f <$> cs
    where f (WorldPair _ fcs) = fmap (toCanonical . flipExtract) fcs
          cs = fmap generateContacts <$> B.culledPairs world
  worldAabbs _ world =
    Aabb <$> IM.elems (B.toAabbs world)
  debugEngineState _ = show . over spSnd toShowableSolverState
  updateWorld _ = SM.updateWorld
