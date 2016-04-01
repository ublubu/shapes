{-# LANGUAGE TypeFamilies #-}

module Physics.Demo.OptWorld where

import Control.Lens
import Control.Monad
import qualified Data.Array.Repa as R
import qualified Data.IntMap.Strict as IM

import qualified Physics.Broadphase.Opt.Aabb as B
import Physics.Contact.Opt
import Physics.Engine.Opt
import qualified Physics.Engine.Opt.Main as OM
import Physics.World.Opt
import Physics.Solvers.Opt (toShowableSolverState)

import Physics.Draw.Canonical
import qualified Physics.Draw.Opt as D
import Physics.Demo.World (Demo(..))

import Utils.Utils

instance Demo Engine where
  type DEngineState Engine = OM.EngineState
  initialEngineState _ = OM.defaultInitialState
  drawWorld _ = D.drawWorld
  demoWorld _ = _spFst
  worldContacts _ world =
    join $ f <$> cs
    where f (WorldPair _ fcs) = fmap (toCanonical . flipExtract) fcs
          cs = fmap generateContacts <$> B.culledPairs world
  worldAabbs _ world =
    (toCanonical . snd) <$> R.toList (B.toAabbs world)
  debugEngineState _ = show . over spSnd toShowableSolverState
  updateWorld _ = OM.updateWorld
