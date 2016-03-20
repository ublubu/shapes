{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

module Shapes.Linear.Template where

import Control.Monad
import Language.Haskell.TH

-- TODO: Use a wrapper type to hold multiple sizes of vector?

makeVectorType :: Name -> Int -> Name -> DecsQ
makeVectorType valueN dim wrapN = do
  let typeN = mkName $ "V" ++ show dim
      constrArg = (NotStrict, ConT valueN)
      definers = [ defineLift
                 , defineLift2
                 ]
  showInst <- deriveShow typeN dim wrapN
  impls <- concat <$> mapM (\f -> f valueN typeN dim) definers
  let decs = [ DataD [] typeN [] [NormalC typeN (replicate dim constrArg)] []
             , showInst
             ] ++ impls
  return decs

data Dummy = Dummy

deriveShow :: Name -> Int -> Name -> DecQ
deriveShow typeN dim wrapN = do
  (pat, vars) <- conPE typeN dim
  let f [] = [| "" |]
      f (v:vs) = [| " " ++ show $(appE (conE wrapN) v) ++ $(f vs) |]
      constructorShown = nameBase typeN
  showClause <- clause [pat] (normalB [| constructorShown ++ $(f vars) |]) []
  [InstanceD [] (AppT showt (ConT _)) [FunD showf _]] <-
    [d| instance Show Dummy where show _ = "text" |]
  return $ InstanceD [] (AppT showt (ConT typeN)) [FunD showf [showClause]]

defineLift :: Name -> Name -> Int -> DecsQ
defineLift valueN typeN dim = do
  (funcP, funcV) <- newPE "f"
  (vecP, elemVars) <- conPE typeN dim
  let liftClause = clause [funcP, vecP] liftBody []
      f = appE funcV
      liftBody = normalB $ appsE (conE typeN : fmap f elemVars)
      liftName = mkName $ "lift" ++ nameBase typeN
      valueT = conT valueN
      typeT = conT typeN
      liftType = arrowsT [arrowsT [valueT, valueT], typeT, typeT]
  funSigDef liftName liftType [liftClause]

defineLift2 :: Name -> Name -> Int -> DecsQ
defineLift2 valueN typeN dim = do
  (funcP, funcV) <- newPE "f"
  (vecP, elemVars) <- conPE typeN dim
  (vecP', elemVars') <- conPE typeN dim
  let pairVars = zip elemVars elemVars'
      liftClause = clause [funcP, vecP, vecP'] liftBody []
      f (x, y) = appsE [funcV, x, y]
      liftBody = normalB $ appsE (conE typeN : fmap f pairVars)
      liftName = mkName $ "lift2" ++ nameBase typeN
      valueT = conT valueN
      typeT = conT typeN
      liftType = arrowsT [arrowsT [valueT, valueT, valueT], typeT, typeT, typeT]
  funSigDef liftName liftType [liftClause]

funSigDef :: Name -> TypeQ -> [ClauseQ] -> DecsQ
funSigDef funN funT funCs = do
  funSig <- sigD funN funT
  funDef <- funD funN funCs
  return [funSig, funDef]

arrowsT :: [TypeQ] -> TypeQ
arrowsT [] = error "can't have no type"
arrowsT [t] = t
arrowsT (t:ts) = appT (appT arrowT t) $ arrowsT ts

newPE :: String -> Q (PatQ, ExpQ)
newPE x = do
  x' <- newName x
  return (varP x', varE x')

conPE :: Name -> Int -> Q (PatQ, [ExpQ])
conPE conN dim = do
  (pats, vars) <- genPE dim
  return (conP conN pats, vars)

genPE :: Int -> Q ([PatQ], [ExpQ])
genPE n = do
  ids <- replicateM n (newName "x")
  return (map varP ids, map varE ids)
