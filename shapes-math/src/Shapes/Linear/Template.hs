{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

module Shapes.Linear.Template where

import Language.Haskell.TH

makeVectorType :: Int -> Name -> Q [Dec]
makeVectorType dim valueN = do
  let typeN = mkName $ "V" ++ show dim
      constrArg = (NotStrict, ConT valueN)
  return [DataD [] typeN [] [NormalC typeN (replicate dim constrArg)] []]
