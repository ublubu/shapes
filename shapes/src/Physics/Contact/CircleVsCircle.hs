{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Physics.Contact.CircleVsCircle where

import           GHC.Types              (Double (..))

import           Data.Either
import           Physics.Contact.Circle
import           Physics.Contact.Types
import           Physics.Linear

generateContacts :: Circle -> Circle -> Maybe Contact'
generateContacts circleA circleB =
  case contact circleA circleB of
    Nothing -> Nothing
    Just Contact {..} ->
      Just (Contact' _contactNormal _contactCenter _contactDepth)
