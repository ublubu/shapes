{-# LANGUAGE TemplateHaskell #-}

{- |
Demo scenes for sample implementation.
TODO: Figure out a good place to put demo/sample stuff without slowing down my development workflow.
-}
module Physics.Scenes.Scene where

import Control.Lens
import Physics.Engine.Class
import Physics.World.Class

data Scene e =
  Scene { _scWorld :: PEWorld' e
        , _scExts :: [External]
        , _scContactBeh :: PEContactBehavior e
        }
makeLenses ''Scene
