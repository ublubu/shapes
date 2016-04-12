{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Physics.Constraint.Types where

import Control.DeepSeq

-- TODO: Why doesn't GeneralizedNewtypeDeriving work in Physics.Constraint?
newtype Lagrangian =
  Lagrangian { _lagrangianVal :: Double } deriving (Show, Num, Eq, Ord, NFData)
