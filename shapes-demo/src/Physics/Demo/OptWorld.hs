{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Physics.Demo.OptWorld where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Maybe
import qualified Data.Vector.Unboxed        as V

import qualified Physics.Broadphase.Aabb    as B
import qualified Physics.Broadphase.Grid    as G
import           Physics.Contact
import           Physics.Contact.ConvexHull
import           Physics.Contact.Types
import           Physics.Engine
import qualified Physics.Engine.Main        as OM
import           Physics.Scenes.Scene
import           Physics.World.Class

import           Physics.Demo.IOWorld       (Demo (..))
import           Physics.Draw.Canonical
import qualified Physics.Draw.Opt           as D

import           Utils.Descending
import           Utils.Utils

instance Demo (Engine ()) where
  type DemoM (Engine ()) = ReaderT OM.EngineConfig (StateT (OM.EngineState () RealWorld) IO)
  runDemo _ scene@Scene{..} action = do
    eState <- liftIO . stToIO $ OM.initEngine scene
    evalStateT (runReaderT action eConfig) eState
    where eConfig = OM.EngineConfig 0.01 _scContactBeh
  resetEngine _ scene =
    convertEngineT $ OM.changeScene scene
  drawWorld p r vt = do
    world <- demoWorld p
    liftIO $ D.drawWorld r vt world
  demoWorld _ = view _1 <$> get
  worldContacts p = do
    world <- demoWorld p
    let cs :: Descending Contact'
        cs = fmap (flipExtractUnsafe . snd) . join $ generateContacts <$> culledPairs
        pairKeys = G.culledKeys (G.toGrid OM.gridAxes world)
        culledPairs = fmap f pairKeys
        f :: (Int, Int) -> (Shape, Shape)
        f ij = fromJust $ iixView (\k -> wObj k . woShape) ij world
    return . _descList $ toCanonical <$> cs
  worldAabbs p = do
    world <- demoWorld p
    return $ toCanonical . snd <$> V.toList (B.toAabbs world)
  debugEngineState _ = return "<insert debug trace here>"
  updateWorld _ = void . convertEngineT $ OM.updateWorld

convertEngineT :: OM.EngineST () RealWorld a -> DemoM (Engine ()) a
convertEngineT action =
  ReaderT (\config -> StateT (\state -> stToIO $ runStateT (runReaderT action config) state))
