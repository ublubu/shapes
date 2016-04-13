{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.World.Object where

import GHC.Generics (Generic)

import Control.DeepSeq
import Control.Lens (makeLenses)
import Physics.Constraint
import Physics.Contact.ConvexHull
import Physics.World.Class

data WorldObj =
  WorldObj { _worldPhysObj :: !PhysicalObj
           , _worldObjMu :: !Double
           , _worldShape :: !ConvexHull
           } deriving (Generic, NFData)
makeLenses ''WorldObj

instance Show WorldObj where
  show (WorldObj obj _ _) = "WorldObj { " ++ show obj ++ " }"

instance Physical WorldObj where
  woPhys = worldPhysObj

instance Contactable WorldObj where
  woMu = worldObjMu
  woShape = worldShape
  woMuShape f obj@WorldObj{..} =
    g <$> f (_worldObjMu, _worldShape)
    where g (mu, shape) =  obj { _worldObjMu = mu
                               , _worldShape = shape
                               }
          {-# INLINE g #-}
  {-# INLINE woMuShape #-}

makeWorldObj :: PhysicalObj -> Double -> ConvexHull -> WorldObj
makeWorldObj phys mu shape = woUpdateShape $ WorldObj phys mu shape
{-# INLINE makeWorldObj #-}
