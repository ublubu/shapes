{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Physics.Constraint.Types where

import Control.DeepSeq
import Control.Lens
import Data.Vector.Unboxed.Deriving

-- TODO: Why doesn't GeneralizedNewtypeDeriving work in Physics.Constraint?
newtype Lagrangian =
  Lagrangian { _lagrangianVal :: Double } deriving (Show, Num, Eq, Ord, NFData)

makeLenses ''Lagrangian

derivingUnbox "Lagrangian"
  [t| Lagrangian -> Double |]
  [| \(Lagrangian l) -> l |]
  [| Lagrangian |]
