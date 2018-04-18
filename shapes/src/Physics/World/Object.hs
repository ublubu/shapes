{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

{- | A physical object that can inhabit a physical world.
Contains a field to hold a reference to something outside
the physical world.
-}
module Physics.World.Object where

import GHC.Generics (Generic)

import Control.DeepSeq
import Control.Lens (makeLenses)
import Physics.Constraint
import Physics.Contact.ConvexHull
import Physics.World.Class

data WorldObj a =
  WorldObj { _worldPhysObj  :: !PhysicalObj
           , _worldObjMu    :: !Double
           , _worldShape    :: !ConvexHull
           , _worldUserData :: !a
           } deriving (Generic, NFData)
makeLenses ''WorldObj

instance Show (WorldObj a) where
  show (WorldObj obj _ _ _) = "WorldObj { " ++ show obj ++ " ... }"

instance Physical (WorldObj a) where
  woPhys = worldPhysObj

instance Contactable (WorldObj a) where
  woMu = worldObjMu
  woShape = worldShape
  woMuShape f obj@WorldObj{..} =
    g <$> f (_worldObjMu, _worldShape)
    where g (mu, shape) =  obj { _worldObjMu = mu
                               , _worldShape = shape
                               }
          {-# INLINE g #-}
  {-# INLINE woMuShape #-}

makeWorldObj :: PhysicalObj -> Double -> ConvexHull -> a -> WorldObj a
makeWorldObj phys mu shape = woUpdateShape . WorldObj phys mu shape
{-# INLINE makeWorldObj #-}
