{-# LANGUAGE DataKinds #-}

module Physics.Contact where

import Control.Lens
import Data.Either
import Linear.Affine
import Linear.Epsilon
import Linear.V
import Linear.V2
import Linear.Vector
import Linear.Matrix
import qualified Physics.Geometry as G
import Physics.Constraint
import Physics.ConstraintSolver
import Physics.Linear
import Physics.Transform
import Utils.Utils

data Contact a = Contact { contactA :: !(PhysicalObj a)
                         , contactB :: !(PhysicalObj a)
                         , contactPoint :: !(P2 a)
                         , contactNormal :: !(V2 a)
                         , contactDepth :: !a
                         , contactIndex :: !(Int, Int) } deriving Show
data ContactBehavior a = ContactBehavior { contactBaumgarte :: !a
                                         , contactPenetrationSlop :: !a } deriving Show

defaultContactBehavior :: (Num a) => ContactBehavior a
defaultContactBehavior = ContactBehavior { contactBaumgarte = 0
                                         , contactPenetrationSlop = 0 }

generateContacts :: (Epsilon a, Floating a, Ord a) => ConstrainedPair a -> [Flipping (Contact a)]
generateContacts cp = case mc of Nothing -> []
                                 Just c -> flipInjectF $ flipMap f c cp
  where shapes = pairMap physicsShape cp
        mc = uncurry G.contact shapes
        f (cc, feat) (a', b') = fmap g ps
          where ps = G.flattenContactPoints cc
                n = G.contactNormal cc
                g p = Contact { contactA = a'
                              , contactB = b'
                              , contactPoint = p ^. G.clens'
                              , contactNormal = n
                              , contactDepth = G.contactDepth feat (p ^. G.clens)
                              , contactIndex = (G.featIndex feat, G.featIndex p)}
