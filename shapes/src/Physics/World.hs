{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Simple data structure that can act as a physical world to simulate.
I will likely implement more interesting world data structures in the future.
-}
module Physics.World where

import           GHC.Generics                 (Generic)

import           Control.DeepSeq
import           Control.Monad.Primitive

import qualified Data.Vector.Mutable          as V
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed.Mutable  as U
import           Physics.Constraint
import           Physics.Contact
import           Physics.Transform
import qualified Utils.EmptiesVector          as E
import           Utils.Utils

-- | An 'External' is a non-constraint effect (e.g. gravity) on physical objects.
type External = Double -> PhysicalObj -> PhysicalObj

data Material = Material
  { _mMu :: Double
  } deriving (Show, Generic, NFData)

derivingUnbox "Material"
  [t|Material -> Double|]
  [|\Material {..} -> _mMu|]
  [|\mu -> Material {_mMu = mu}|]

data World s label = World
  { _wPhysObjs  :: U.MVector s PhysicalObj
  , _wLabels    :: V.MVector s label
  , _wMaterials :: U.MVector s Material
  , _wShapes    :: V.MVector s Shape
  , _wEmpties   :: E.EmptiesVector s
  }

data WorldObj label = WorldObj
  { _woPhysObj  :: PhysicalObj
  , _woLabel    :: label
  , _woMaterial :: Material
  , _woShape    :: Shape
  }

new :: (PrimMonad m) => Int -> m (World (PrimState m) label)
new capacity = do
  physObjs <- U.new capacity
  labels <- V.new capacity
  materials <- U.new capacity
  shapes <- V.new capacity
  empties <- E.new capacity
  return
    World
    { _wPhysObjs = physObjs
    , _wLabels = labels
    , _wMaterials = materials
    , _wShapes = shapes
    , _wEmpties = empties
    }
{-# INLINE new #-}

append :: (PrimMonad m) => World (PrimState m) label -> WorldObj label -> m Int
append World {..} WorldObj {..} = do
  i <- E.append _wEmpties
  U.write _wPhysObjs i _woPhysObj
  V.write _wLabels i _woLabel
  U.write _wMaterials i _woMaterial
  V.write _wShapes i _woShape
  return i
{-# INLINE append #-}

delete :: (PrimMonad m) => World (PrimState m) label -> Int -> m ()
delete World {..} = E.safeDelete _wEmpties
{-# INLINE delete #-}

-- | How many objects are in the world. ("filled" slots)
filled :: (PrimMonad m) => World (PrimState m) label -> m Int
filled World {..} = E.filled _wEmpties
{-# INLINE filled #-}

readPhysObj ::
     (PrimMonad m) => World (PrimState m) label -> Int -> m PhysicalObj
readPhysObj World {..} = U.read _wPhysObjs
{-# INLINE readPhysObj #-}

updatePhysObj ::
     (PrimMonad m) => World (PrimState m) label -> Int -> PhysicalObj -> m ()
updatePhysObj World {..} = U.write _wPhysObjs
{-# INLINE updatePhysObj #-}

readLabel :: (PrimMonad m) => World (PrimState m) label -> Int -> m label
readLabel World {..} = V.read _wLabels
{-# INLINE readLabel #-}

updateLabel ::
     (PrimMonad m) => World (PrimState m) label -> Int -> label -> m ()
updateLabel World {..} = V.write _wLabels
{-# INLINE updateLabel #-}

readMaterial :: (PrimMonad m) => World (PrimState m) label -> Int -> m Material
readMaterial World {..} = U.read _wMaterials
{-# INLINE readMaterial #-}

updateMaterial ::
     (PrimMonad m) => World (PrimState m) label -> Int -> Material -> m ()
updateMaterial World {..} = U.write _wMaterials
{-# INLINE updateMaterial #-}

readShape :: (PrimMonad m) => World (PrimState m) label -> Int -> m Shape
readShape World {..} = V.read _wShapes
{-# INLINE readShape #-}

updateShape ::
     (PrimMonad m) => World (PrimState m) label -> Int -> Shape -> m ()
updateShape World {..} = V.write _wShapes
{-# INLINE updateShape #-}

-- | Create a 'World' from a list of inhabitants
fromList ::
     (PrimMonad m)
  => [WorldObj label] -- ^ Population for the new 'World'
  -> m (World (PrimState m) label)
fromList objs = do
  world <- new (length objs)
  mapM_ (append world) objs
  return world
{-# INLINE fromList #-}

{- |
Update the shape of an object to match its current physical state.

By keeping all shapes in world space, we ensure that each shape
only needs to be transformed once per frame.
-}
moveShape :: PhysicalObj -> Shape -> Shape
moveShape physObj =
  flip setShapeTransform (transform $ _physObjTransform physObj)
{-# INLINE moveShape #-}

moveShapes :: (PrimMonad m) => World (PrimState m) label -> m ()
moveShapes World {..} = E.mapM_ f _wEmpties
  where f i = do
          physObj <- U.read _wPhysObjs i
          V.modify _wShapes (moveShape physObj) i
{-# INLINE moveShapes #-}

makeWorldObj :: PhysicalObj -> Double -> Shape -> label -> WorldObj label
makeWorldObj physObj mu shape label =
  WorldObj
  { _woPhysObj = physObj
  , _woLabel = label
  , _woMaterial = Material {_mMu = mu}
  , _woShape = moveShape physObj shape
  }
{-# INLINE makeWorldObj #-}

{- |
Apply 'External' effects to the objects in a world.

This happens each frame before constraints are created and solved.
-}
applyExternal ::
     (PrimMonad m) => External -> Double -> World (PrimState m) label -> m ()
applyExternal f_ dt World {..} = E.mapM_ f _wEmpties
  where f = U.modify _wPhysObjs (f_ dt)
{-# INLINE applyExternal #-}


{- |
Advance the 'PhysicalObj's of the world by a given time delta
using the current velocity of each.

Does not move the 'Shape's to match their physical state.
-}
advance :: (PrimMonad m) => Double -> World (PrimState m) label -> m ()
advance dt World {..} = E.mapM_ f _wEmpties
  where f = U.modify _wPhysObjs (`advanceObj` dt)
{-# INLINE advance #-}

readPhysObjPair ::
     (PrimMonad m)
  => World (PrimState m) label
  -> (Int, Int)
  -> m (PhysicalObj, PhysicalObj)
readPhysObjPair world (i, j) = do
  obj_i <- readPhysObj world i
  obj_j <- readPhysObj world j
  return (obj_i, obj_j)
{-# INLINE readPhysObjPair #-}

updatePhysObjPair ::
     (PrimMonad m)
  => World (PrimState m) label
  -> (Int, Int)
  -> (PhysicalObj, PhysicalObj)
  -> m ()
updatePhysObjPair world (i, j) (a, b) = do
  updatePhysObj world i a
  updatePhysObj world j b
{-# INLINE updatePhysObjPair #-}

modifyPhysObjPair ::
     (PrimMonad m)
  => World (PrimState m) label
  -> ((PhysicalObj, PhysicalObj) -> (PhysicalObj, PhysicalObj))
  -> (Int, Int)
  -> m ()
modifyPhysObjPair world f ij = do
  ab <- readPhysObjPair world ij
  updatePhysObjPair world ij (f ab)
{-# INLINE modifyPhysObjPair #-}

-- TODO: change to readMaterialPair
readMuPair :: (PrimMonad m) => World (PrimState m) label -> (Int, Int) -> m (Double, Double)
readMuPair world (i, j) = do
  u_i <- _mMu <$> readMaterial world i
  u_j <- _mMu <$> readMaterial world j
  return (u_i, u_j)
{-# INLINE readMuPair #-}
