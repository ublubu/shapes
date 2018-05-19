{-# LANGUAGE TemplateHaskell #-}

{- |
Demo scenes for sample implementation.
TODO: Figure out a good place to put demo/sample stuff without slowing down my development workflow.
-}
module Physics.Scenes.Scene where

import Control.Lens
import Physics.World.Class
import Physics.World
import Physics.World.Object
import Physics.Contact.Types

data Scene usr =
  Scene { _scWorld :: World (WorldObj usr)
        , _scExts :: [External]
        , _scContactBeh :: ContactBehavior
        }
makeLenses ''Scene
