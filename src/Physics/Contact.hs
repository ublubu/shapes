{-# LANGUAGE DataKinds #-}

module Physics.Contact where

import Linear.V
import Linear.V2
import Physics.Linear

data ContactObject a = ContactObject { contactObjectCenter :: V2 a
                                     , contactObjectVel :: V2 a
                                     , contactObjectRotVel :: a
                                     , contactObjectContactRel :: V2 a }

data Contact a = Contact { contactObjectA :: ContactObject a
                         , contactObjectB :: ContactObject a
                         , contactNormal :: V2 a }

jacobian :: Num a => Contact a -> V6 a
jacobian (Contact (ContactObject xa va wa ra) (ContactObject xb vb wb rb) n) =
  ja `join33` jb
  where ja = (-n) `append2` ((-ra) `cross22` n)
        jb = n `append2` (rb `cross22` n)

velocity :: Contact a -> V 6 a
velocity (Contact (ContactObject xa va wa ra) (ContactObject xb vb wb rb) n) =
  (va `append2` wa) `join33` (vb `append2` wb)

