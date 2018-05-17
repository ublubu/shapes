{-# LANGUAGE MagicHash #-}

module Physics.Contact.Circle.Benchmark where

import Physics.Contact.Circle
import Physics.Linear

test =
  contact
    (Circle (P2 $ V2 (-1.0##) 0.0##) 1.1)
    (Circle (P2 $ V2 1.0## 0.0##) 1.1)
