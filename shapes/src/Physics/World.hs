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
import           Control.Monad.ST
import           Data.STRef

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

new :: Int -> ST s (World s label)
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

append :: World s label -> WorldObj label -> ST s Int
append World {..} WorldObj {..} = do
  i <- E.append _wEmpties
  U.write _wPhysObjs i _woPhysObj
  V.write _wLabels i _woLabel
  U.write _wMaterials i _woMaterial
  V.write _wShapes i _woShape
  return i

delete :: World s label -> Int -> ST s ()
delete World {..} = E.safeDelete _wEmpties

-- | How many objects are in the world. ("filled" slots)
filled :: World s label -> ST s Int
filled World {..} = E.filled _wEmpties

readPhysObj :: World s label -> Int -> ST s PhysicalObj
readPhysObj World {..} = U.read _wPhysObjs

updatePhysObj :: World s label -> Int -> PhysicalObj -> ST s ()
updatePhysObj World {..} = U.write _wPhysObjs

readLabel :: World s label -> Int -> ST s label
readLabel World {..} = V.read _wLabels

updateLabel :: World s label -> Int -> label -> ST s ()
updateLabel World {..} = V.write _wLabels

readMaterial :: World s label -> Int -> ST s Material
readMaterial World {..} = U.read _wMaterials

updateMaterial :: World s label -> Int -> Material -> ST s ()
updateMaterial World {..} = U.write _wMaterials

readShape :: World s label -> Int -> ST s Shape
readShape World {..} = V.read _wShapes

updateShape :: World s label -> Int -> Shape -> ST s ()
updateShape World {..} = V.write _wShapes

-- | Create a 'World' from a list of inhabitants
fromList ::
     [WorldObj label] -- ^ Population for the new 'World'
  -> ST s (World s label)
fromList objs = do
  world <- new (length objs)
  mapM_ (append world) objs
  return world

{- |
Update the shape of an object to match its current physical state.

By keeping all shapes in world space, we ensure that each shape
only needs to be transformed once per frame.
-}
moveShape :: PhysicalObj -> Shape -> Shape
moveShape physObj =
  flip setShapeTransform (transform $ _physObjTransform physObj)

moveShapes :: World s label -> ST s ()
moveShapes World {..} = E.mapM_ f _wEmpties
  where f i = do
          physObj <- U.read _wPhysObjs i
          V.modify _wShapes (moveShape physObj) i

makeWorldObj :: PhysicalObj -> Double -> Shape -> label -> WorldObj label
makeWorldObj physObj mu shape label =
  WorldObj
  { _woPhysObj = physObj
  , _woLabel = label
  , _woMaterial = Material {_mMu = mu}
  , _woShape = moveShape physObj shape
  }

{- |
Apply 'External' effects to the objects in a world.

This happens each frame before constraints are created and solved.
-}
applyExternal :: External -> Double -> World s label -> ST s ()
applyExternal f_ dt World {..} = E.mapM_ f _wEmpties
  where f = U.modify _wPhysObjs (f_ dt)


{- |
Advance the 'PhysicalObj's of the world by a given time delta
using the current velocity of each.

Does not move the 'Shape's to match their physical state.
-}
advance :: Double -> World s label -> ST s ()
advance dt World {..} = E.mapM_ f _wEmpties
  where f = U.modify _wPhysObjs (`advanceObj` dt)

readPhysObjPair :: World s label -> (Int, Int) -> ST s (PhysicalObj, PhysicalObj)
readPhysObjPair world (i, j) = do
  obj_i <- readPhysObj world i
  obj_j <- readPhysObj world j
  return (obj_i, obj_j)

updatePhysObjPair ::
  World s label
  -> (Int, Int)
  -> (PhysicalObj, PhysicalObj)
  -> ST s ()
updatePhysObjPair world (i, j) (a, b) = do
  updatePhysObj world i a
  updatePhysObj world j b

modifyPhysObjPair ::
     World s label
  -> ((PhysicalObj, PhysicalObj) -> (PhysicalObj, PhysicalObj))
  -> (Int, Int)
  -> ST s ()
modifyPhysObjPair world f ij = do
  ab <- readPhysObjPair world ij
  updatePhysObjPair world ij (f ab)

-- TODO: change to readMaterialPair
readMuPair :: World s label -> (Int, Int) -> ST s (Double, Double)
readMuPair world (i, j) = do
  u_i <- _mMu <$> readMaterial world i
  u_j <- _mMu <$> readMaterial world j
  return (u_i, u_j)
