{-# LANGUAGE TemplateHaskell #-}

{- |
Demo scenes for sample implementation.
TODO: Figure out a good place to put demo/sample stuff without slowing down my development workflow.
-}
module Physics.Scenes.Scene where

import Control.Lens
import Physics.World
import Physics.Contact.Types

data Scene s label =
  Scene { _scWorld :: World s label
        , _scExts :: External
        , _scContactBeh :: ContactBehavior
        }
makeLenses ''Scene
