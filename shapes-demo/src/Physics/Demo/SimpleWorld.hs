{-# LANGUAGE TypeFamilies #-}

module Physics.Demo.SimpleWorld where

import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM

import qualified Physics.Broadphase.Simple.Aabb as B
import Physics.Contact.Simple
import Physics.Engine.Simple
import qualified Physics.Engine.Simple.Main as SM
import Physics.World.Simple
import Physics.Solvers.Simple (toShowableSolverState)

import Physics.Draw.Canonical
import qualified Physics.Draw.Simple as D
import Physics.Demo.IOWorld (Demo(..))

import Utils.Utils

instance Demo SimpleEngine where
  type DemoM SimpleEngine = StateT SM.EngineState IO
  runDemo _ scene action =
    evalStateT action $ SM.defaultInitialState scene
  resetEngine _ scene =
    put $ SM.defaultInitialState scene
  drawWorld p r vt = do
    world <- demoWorld p
    liftIO $ D.drawWorld r vt world
  demoWorld _ = SM._esWorld <$> get
  worldContacts p = do
    world <- demoWorld p
    let f (WorldPair _ fcs) = fmap (toCanonical . flipExtractUnsafe) fcs
        cs = fmap generateContacts <$> B.culledPairs world
    return $ join $ f <$> cs
  worldAabbs p = do
    world <- demoWorld p
    return $ Aabb <$> IM.elems (B.toAabbs world)
  debugEngineState _ = show . toShowableSolverState . SM._esSolver <$> get
  updateWorld _ dt = do
    eState <- get
    put $ SM.updateWorld dt eState

{-
instance Demo SimpleEngine where
  type DEngineState SimpleEngine = SM.EngineState
  initialEngineState _ = SM.defaultInitialState
  drawWorld _ = D.drawWorld
  demoWorld _ = _spFst
  worldContacts _ world =
    join $ f <$> cs
    where f (WorldPair _ fcs) = fmap (toCanonical . flipExtractUnsafe) fcs
          cs = fmap generateContacts <$> B.culledPairs world
  worldAabbs _ world =
    Aabb <$> IM.elems (B.toAabbs world)
  debugEngineState _ = show . over spSnd toShowableSolverState
  updateWorld _ = SM.updateWorld
-}
