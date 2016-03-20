{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

module Shapes.Linear.Template where

import Control.Monad
import Language.Haskell.TH

makeVectorType :: Int -> Name -> Name -> Q [Dec]
makeVectorType dim valueN wrapN = do
  let typeN = mkName $ "V" ++ show dim
      constrArg = (NotStrict, ConT valueN)
  showInst <- deriveShow dim typeN wrapN
  return [ DataD [] typeN [] [NormalC typeN (replicate dim constrArg)] []
         , showInst
         ]

data Dummy = Dummy

deriveShow :: Int -> Name -> Name -> Q Dec
deriveShow dim typeN wrapN = do
  (pats, vars) <- genPE dim
  let f [] = [| "" |]
      f (v:vs) = [| " " ++ show $(AppE (ConE wrapN) <$> v) ++ $(f vs) |]
      constructorShown = nameBase typeN
  showClause <- clause [conP typeN pats] (normalB [| constructorShown ++ $(f vars) |]) []
  [InstanceD [] (AppT showt (ConT _)) [FunD showf _]] <-
    [d| instance Show Dummy where show _ = "text" |]
  return $ InstanceD [] (AppT showt (ConT typeN)) [FunD showf [showClause]]

genPE :: Int -> Q ([PatQ], [ExpQ])
genPE n = do
  ids <- replicateM n (newName "x")
  return (map varP ids, map varE ids)
