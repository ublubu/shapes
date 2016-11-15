{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Just pretend this module is part of Physics.Constraint.
Last time I checked, GeneralizedNewtypeDeriving didn't work on Lagrangian in Physics.Constraint.
-}
module Physics.Constraint.Types where

import Control.DeepSeq
import Control.Lens
import Data.Vector.Unboxed.Deriving

-- TODO: Why doesn't GeneralizedNewtypeDeriving work in Physics.Constraint?
-- | Lagrangian multiplier - the (signed) magnitude of the impulse
-- required to solve a constraint equation.
newtype Lagrangian =
  Lagrangian { _lagrangianVal :: Double } deriving (Show, Num, Eq, Ord, NFData)

makeLenses ''Lagrangian

derivingUnbox "Lagrangian"
  [t| Lagrangian -> Double |]
  [| \(Lagrangian l) -> l |]
  [| Lagrangian |]
