{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

module Shapes.Linear.Template where

import Control.Monad
import Language.Haskell.TH

-- TODO: Use a wrapper type to hold multiple sizes of vector?

data ValueInfo = ValueInfo { _valueN :: Name
                           , _valueWrap :: Name
                           , _valueBoxed :: Name
                           , _valueAdd :: Name
                           , _valueSub :: Name
                           , _valueMul :: Name
                           , _valueDiv :: Name
                           , _valueNeg :: Name
                           , _valueEq :: Name
                           , _valueNeq :: Name
                           , _valueLeq :: Name
                           , _valueGeq :: Name
                           , _valueGt :: Name
                           , _valueLt :: Name
                           }

makeVectorType :: ValueInfo -> Int -> DecsQ
makeVectorType vi@ValueInfo{..} dim = do
  let vectorN = mkName $ "V" ++ show dim
      constrArg = (NotStrict, ConT _valueN)
      definers = [ defineLift
                 , defineLift2
                 , defineDot
                 , defineFromList
                 ]
  showInst <- deriveShow vectorN vi dim
  impls <- concat <$> mapM (\f -> f vectorN vi dim) definers
  let decs = [ DataD [] vectorN [] [NormalC vectorN (replicate dim constrArg)] []
             , showInst
             ] ++ impls
  return decs

data Dummy = Dummy

deriveShow :: Name -> ValueInfo -> Int -> DecQ
deriveShow vectorN ValueInfo{..} dim = do
  (pat, vars) <- conPE vectorN "a" dim
  let f [] = [| "" |]
      f (v:vs) = [| " " ++ show $(appE (conE _valueWrap) v) ++ $(f vs) |]
      constructorShown = nameBase vectorN
  showClause <- clause [pat] (normalB [| constructorShown ++ $(f vars) |]) []
  [InstanceD [] (AppT showt (ConT _)) [FunD showf _]] <-
    [d| instance Show Dummy where show _ = "text" |]
  return $ InstanceD [] (AppT showt (ConT vectorN)) [FunD showf [showClause]]

defineLift :: Name -> ValueInfo -> Int -> DecsQ
defineLift vectorN ValueInfo{..} dim = do
  (funcP, funcV) <- newPE "f"
  (vecP, elemVars) <- conPE vectorN "a" dim
  let liftClause = clause [funcP, vecP] liftBody []
      f = appE funcV
      liftBody = normalB $ appsE (conE vectorN : fmap f elemVars)
      liftName = mkName $ "lift" ++ nameBase vectorN
      valueT = conT _valueN
      vectorT = conT vectorN
      liftType = arrowsT [arrowsT [valueT, valueT], vectorT, vectorT]
  funSigDef liftName liftType [liftClause]

defineLift2 :: Name -> ValueInfo -> Int -> DecsQ
defineLift2 vectorN ValueInfo{..} dim = do
  (funcP, funcV) <- newPE "f"
  (vecP, elemVars) <- conPE vectorN "a" dim
  (vecP', elemVars') <- conPE vectorN "b" dim
  let pairVars = zip elemVars elemVars'
      liftClause = clause [funcP, vecP, vecP'] liftBody []
      f (x, y) = appsE [funcV, x, y]
      liftBody = normalB $ appsE (conE vectorN : fmap f pairVars)
      liftName = mkName $ "lift2" ++ nameBase vectorN
      valueT = conT _valueN
      vectorT = conT vectorN
      liftType = arrowsT [arrowsT [valueT, valueT, valueT], vectorT, vectorT, vectorT]
  funSigDef liftName liftType [liftClause]

defineDot :: Name -> ValueInfo -> Int -> DecsQ
defineDot vectorN ValueInfo{..} dim = do
  (vecP, elemVars) <- conPE vectorN "a" dim
  (vecP', elemVars') <- conPE vectorN "b" dim
  let pairVars = zip elemVars elemVars'
      products = (uncurry $ infixApp' (varE _valueMul)) <$> pairVars
      sum' = foldl1 (infixApp' $ varE _valueAdd) products
      dotClause = clause [vecP, vecP'] (normalB sum') []
      dotName = mkName $ "dot" ++ nameBase vectorN
      valueT = conT _valueN
      vectorT = conT vectorN
      dotType = arrowsT [vectorT, vectorT, valueT]
  funSigDef dotName dotType [dotClause]

defineFromList :: Name -> ValueInfo -> Int -> DecsQ
defineFromList vectorN ValueInfo{..} dim = do
  (pats, vars) <- genPEWith "x" dim (conP _valueWrap . return . varP) varE
  let listPat = listP pats
      vecE = appsE (conE vectorN : vars)
      fromListClause0 = clause [listPat] (normalB vecE) []
      fromListClause1 = clause [wildP] (normalB [| error "wrong number of elements" |]) []
      fromListName = mkName $ "fromList" ++ nameBase vectorN
      vectorT = conT vectorN
      argT = appT listT (conT _valueBoxed)
      fromListType = arrowsT [argT, vectorT]
  funSigDef fromListName fromListType [fromListClause0, fromListClause1]

infixApp' :: ExpQ -> ExpQ -> ExpQ -> ExpQ
infixApp' = flip infixApp

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

conPE :: Name -> String -> Int -> Q (PatQ, [ExpQ])
conPE conN x dim = do
  (pats, vars) <- genPE x dim
  return (conP conN pats, vars)

genPEWith :: String -> Int -> (Name -> PatQ) -> (Name -> ExpQ) -> Q ([PatQ], [ExpQ])
genPEWith x n mkP mkE = do
  ids <- replicateM n (newName x)
  return (fmap mkP ids, fmap mkE ids)

genPE :: String -> Int -> Q ([PatQ], [ExpQ])
genPE x n = genPEWith x n varP varE
