{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Shapes.Linear.Template where

import Test.QuickCheck.Arbitrary

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

makeInlineD :: Name -> DecQ
makeInlineD n = pragInlD n Inline FunLike AllPhases

makeVectorN :: Int -> Name
makeVectorN dim = mkName $ "V" ++ show dim

makeVectorType :: ValueInfo -> Int -> DecsQ
makeVectorType vi@ValueInfo{..} dim = do
  let vectorN = makeVectorN dim
-- #if MIN_VERSION_base(4,9,0)
--       constrArg = (notStrict, ConT _valueN)
-- #else
      constrArg = (NotStrict, ConT _valueN)
-- #endif
      definers = [ defineLift
                 , defineLift2
                 , defineDot
                 , defineFromList
                 , defineToList
                 , deriveShow
                 , deriveArbitrary
                 ]
  impls <- concat <$> mapM (\f -> f vectorN vi dim) definers
  let decs = DataD [] vectorN [] [NormalC vectorN (replicate dim constrArg)] [] : impls
  return decs

deriveShow :: Name -> ValueInfo -> Int -> DecsQ
deriveShow vectorN ValueInfo{..} dim = do
  (pat, vars) <- conPE vectorN "a" dim
  let f [] = [| "" |]
      f (v:vs) = [| " " ++ show $(appE (conE _valueWrap) v) ++ $(f vs) |]
      constructorShown = nameBase vectorN
      showClause = clause [pat] (normalB [| constructorShown ++ $(f vars) |]) []
  return <$> instanceD (cxt []) (appT (conT ''Show) (conT vectorN)) [funD 'show [showClause]]

dimE :: Int -> ExpQ
dimE = litE . integerL . fromIntegral

deriveArbitrary :: Name -> ValueInfo -> Int -> DecsQ
deriveArbitrary vectorN ValueInfo{..} dim = do
  let arbClause = clause [] (normalB $ infixApp (fromListE vectorN) (varE '(<$>)) arbList) []
      arbList = [| replicateM $(dimE dim) arbitrary |]
  return <$> instanceD (cxt []) (appT (conT ''Arbitrary) (conT vectorN)) [funD 'arbitrary [arbClause]]

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
  inlSigDef liftName liftType [liftClause]

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
  inlSigDef liftName liftType [liftClause]

dotE :: ValueInfo -> [ExpQ] -> [ExpQ] -> ExpQ
dotE ValueInfo{..} row col = foldl1 (infixApp' $ varE _valueAdd) products
  where products = uncurry (infixApp' $ varE _valueMul) <$> zip row col

defineDot :: Name -> ValueInfo -> Int -> DecsQ
defineDot vectorN vi@ValueInfo{..} dim = do
  (vecP, elemVars) <- conPE vectorN "a" dim
  (vecP', elemVars') <- conPE vectorN "b" dim
  let dotClause = clause [vecP, vecP'] (normalB $ dotE vi elemVars elemVars') []
      dotName = mkName $ "dot" ++ nameBase vectorN
      valueT = conT _valueN
      vectorT = conT vectorN
      dotType = arrowsT [vectorT, vectorT, valueT]
  inlSigDef dotName dotType [dotClause]

defineJoinSplit :: ValueInfo -> (Int, Int) -> DecsQ
defineJoinSplit ValueInfo{..} (left, right) = do
  let vecN = makeVectorN left
      vecN' = makeVectorN right
      vecN'' = makeVectorN (left + right)
  (vecP, elemVs) <- conPE vecN "a" left
  (vecP', elemVs') <- conPE vecN' "b" right
  (vecP'', elemVs'') <- conPE vecN'' "c" (left + right)
  let joinE = appsE (conE vecN'' : elemVs ++ elemVs')
      joinC = simpleClause [vecP, vecP'] joinE
      joinN = mkName $ "join" ++ show left ++ "v" ++ show right
      joinT = arrowsT [vecT, vecT', vecT'']
      (leftVs, rightVs) = splitAt left elemVs''
      splitE = tupE [ appsE $ conE vecN : leftVs
                    , appsE $ conE vecN' : rightVs
                    ]
      splitC = simpleClause [vecP''] splitE
      splitN = mkName $ "split" ++ show left ++ "v" ++ show right
      splitT = arrowsT [vecT'', tupT [vecT, vecT']]
      vecT = conT vecN
      vecT' = conT vecN'
      vecT'' = conT vecN''
  joinI <- inlSigDef joinN joinT [joinC]
  splitI <- inlSigDef splitN splitT [splitC]
  return $ joinI ++ splitI

fromListN :: Name -> Name
fromListN = mkName . ("fromList" ++) . nameBase

fromListE :: Name -> ExpQ
fromListE = varE . fromListN

defineFromList :: Name -> ValueInfo -> Int -> DecsQ
defineFromList vectorN ValueInfo{..} dim = do
  (pats, vars) <- genPEWith "x" dim (conP _valueWrap . return . varP) varE
  let listPat = listP pats
      vecE = appsE (conE vectorN : vars)
      fromListClause0 = clause [listPat] (normalB vecE) []
      fromListClause1 = clause [wildP] (normalB [| error "wrong number of elements" |]) []
      vectorT = conT vectorN
      argT = appT listT (conT _valueBoxed)
      fromListType = arrowsT [argT, vectorT]
  inlSigDef (fromListN vectorN) fromListType [fromListClause0, fromListClause1]

defineToList :: Name -> ValueInfo -> Int -> DecsQ
defineToList vectorN ValueInfo{..} dim = do
  (vecP, elemVars) <- conPE vectorN "a" dim
  let boxedElemVars = fmap (appE $ conE _valueWrap) elemVars
      toListClause = clause [vecP] (normalB $ listE boxedElemVars) []
      toListName = mkName $ "toList" ++ nameBase vectorN
      vectorT = conT vectorN
      resultT = appT listT (conT _valueBoxed)
      toListType = arrowsT [vectorT, resultT]
  inlSigDef toListName toListType [toListClause]

infixApp' :: ExpQ -> ExpQ -> ExpQ -> ExpQ
infixApp' = flip infixApp

inlSigDef :: Name -> TypeQ -> [ClauseQ] -> DecsQ
inlSigDef funN funT funCs = do
  sigdef <- funSigDef funN funT funCs
  inl <- makeInlineD funN
  return $ sigdef ++ [inl]

funSigDef :: Name -> TypeQ -> [ClauseQ] -> DecsQ
funSigDef funN funT funCs = do
  funSig <- sigD funN funT
  funDef <- funD funN funCs
  return [funSig, funDef]

tupT :: [TypeQ] -> TypeQ
tupT ts = foldl appT (tupleT $ length ts) ts

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

simpleClause :: [PatQ] -> ExpQ -> ClauseQ
simpleClause ps e = clause ps (normalB e) []
