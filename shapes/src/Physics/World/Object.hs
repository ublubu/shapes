{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | A physical object that can inhabit a physical world.
Contains a field to hold a reference to something outside
the physical world.
-}
module Physics.World.Object where

import           GHC.Generics       (Generic)

import           Control.DeepSeq
import           Control.Lens       (Lens', makeLenses, view, (%~), (&))
import           Physics.Constraint
import           Physics.Contact
import           Physics.Transform  (transform)

data WorldObj a =
  WorldObj { _worldPhysObj  :: !PhysicalObj
           , _worldObjMu    :: !Double
           , _worldShape    :: !Shape
           , _worldUserData :: !a
           } deriving (Generic, NFData)
makeLenses ''WorldObj

instance Show (WorldObj a) where
  show (WorldObj obj _ _ _) = "WorldObj { " ++ show obj ++ " ... }"

-- | Lens for the embedded 'PhysicalObj'
woPhys :: Lens' (WorldObj a) PhysicalObj
woPhys = worldPhysObj

-- | Lens for embedded coefficient of friction \"mu\"
woMu :: Lens' (WorldObj a) Double
woMu = worldObjMu

-- | Lens for embedded contact shape
woShape :: Lens' (WorldObj a) Shape
woShape = worldShape

-- | Lens for embedded pair of (coefficient of friction, contact shape)
woMuShape :: Lens' (WorldObj a) (Double, Shape)
woMuShape f obj@WorldObj{..} =
  g <$> f (_worldObjMu, _worldShape)
  where g (mu, shape) =  obj { _worldObjMu = mu
                             , _worldShape = shape
                             }

makeWorldObj :: PhysicalObj -> Double -> Shape -> a -> WorldObj a
makeWorldObj phys mu shape = woUpdateShape . WorldObj phys mu shape
{-# INLINE makeWorldObj #-}

-- | Update the shape of an object to match its current physical state.
--
-- By keeping all shapes in world space, we ensure that each shape
-- only needs to be transformed once per frame.
woUpdateShape :: WorldObj a -> WorldObj a
woUpdateShape obj =
  obj & woShape %~ flip setShapeTransform (transform t)
  where t = _physObjTransform . view woPhys $ obj
