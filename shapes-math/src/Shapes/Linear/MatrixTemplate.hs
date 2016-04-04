{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

module Shapes.Linear.MatrixTemplate where

import Data.Monoid
import Language.Haskell.TH

import Shapes.Linear.Template

makeMatrixNL :: (Int, Int) -> (Name, Int)
makeMatrixNL (rows, cols) =
  (mkName $ "M" ++ show rows ++ "x" ++ show cols, rows * cols)

makeMatrixType :: ValueInfo -> (Int, Int) -> DecsQ
makeMatrixType vi@ValueInfo{..} dims = do
  let (matrixN, len) = makeMatrixNL dims
      constrArg = strictType notStrict (conT _valueN)
      definers = [ defineLift
                 , defineLift2
                 , defineFromList
                 , defineToList
                 , deriveShow
                 , deriveArbitrary
                 ]
      definers' = [ defineMatrixMulVector
                  , defineVectorMulMatrix
                  , defineDiagMulMatrix
                  , defineMatrixMulDiag
                  , defineVectorOuterProduct
                  ]
  impls <- concat <$> mapM (\f -> f matrixN vi len) definers
  impls' <- concat <$> mapM (\f -> f vi dims) definers'
  matrixD <- dataD (cxt []) matrixN [] [normalC matrixN (replicate len constrArg)] []
  return $ matrixD : impls ++ impls'

defineMatrixMul :: ValueInfo -> (Int, Int, Int) -> DecsQ
defineMatrixMul vi@ValueInfo{..} (left, inner, right) = do
  let (matN, len) = makeMatrixNL (left, inner)
      (matN', len') = makeMatrixNL (inner, right)
      (matN'', _) = makeMatrixNL (left, right)
  (matP, elemVars) <- conPE matN "a" len
  (matP', elemVars') <- conPE matN "b" len'
  let rows = chunks inner elemVars
      cols = stripes right elemVars'
      dotEs = do
        row <- rows
        col <- cols
        return $ dotE vi row col
      resultE = appsE (conE matN'' : dotEs)
      mulN = mkName $ "mul" ++ show left ++ "x" ++ show inner ++ "x" ++ show right
      mulC = simpleClause [matP, matP'] resultE
      mulT = arrowsT [matT, matT', matT'']
      matT = conT matN
      matT' = conT matN'
      matT'' = conT matN''
  inlSigDef mulN mulT [mulC]

defineMatrixMulVector :: ValueInfo -> (Int, Int) -> DecsQ
defineMatrixMulVector vi@ValueInfo{..} dims@(left, inner) = do
  let (matN, len) = makeMatrixNL dims
      vecN = makeVectorN inner
      vecN' = makeVectorN left
  (matP, elemVars) <- conPE matN "a" len
  (vecP, col) <- conPE vecN "b" inner
  let rows = chunks inner elemVars
      dotEs = do
        row <- rows
        return $ dotE vi row col
      resultE = appsE (conE vecN' : dotEs)
      mulN = mkName $ "mul" ++ show left ++ "x" ++ show inner ++ "c"
      mulC = simpleClause [matP, vecP] resultE
      mulT = arrowsT [matT, vecT, vecT']
      matT = conT matN
      vecT = conT vecN
      vecT' = conT vecN'
  inlSigDef mulN mulT [mulC]

defineVectorMulMatrix :: ValueInfo -> (Int, Int) -> DecsQ
defineVectorMulMatrix vi@ValueInfo{..} dims@(inner, right) = do
  let vecN = makeVectorN inner
      (matN, len) = makeMatrixNL dims
      vecN' = makeVectorN right
  (vecP, row) <- conPE vecN "a" inner
  (matP, elemVars) <- conPE matN "b" len
  let cols = stripes right elemVars
      dotEs = do
        col <- cols
        return $ dotE vi row col
      resultE = appsE (conE vecN' : dotEs)
      mulN = mkName $ "mulr" ++ show inner ++ "x" ++ show right
      mulC = simpleClause [vecP, matP] resultE
      mulT = arrowsT [vecT, matT, vecT']
      vecT = conT vecN
      matT = conT matN
      vecT' = conT vecN'
  inlSigDef mulN mulT [mulC]

defineDiagMulMatrix :: ValueInfo -> (Int, Int) -> DecsQ
defineDiagMulMatrix ValueInfo{..} dims@(inner, right) = do
  let vecN = makeVectorN inner
      (matN, len) = makeMatrixNL dims
  (vecP, diag) <- conPE vecN "a" inner
  (matP, elemVars) <- conPE matN "b" len
  let rows = chunks right elemVars
      rowE scalar = fmap (infixApp' (varE _valueMul) scalar)
      rowEs = zipWith rowE diag rows
      resultE = appsE (conE matN : concat rowEs)
      mulN = mkName $ "muld" ++ show inner ++ "x" ++ show right
      mulC = simpleClause [vecP, matP] resultE
      mulT = arrowsT [vecT, matT, matT]
      vecT = conT vecN
      matT = conT matN
  inlSigDef mulN mulT [mulC]

defineMatrixMulDiag :: ValueInfo -> (Int, Int) -> DecsQ
defineMatrixMulDiag ValueInfo{..} dims@(left, inner) = do
  let vecN = makeVectorN inner
      (matN, len) = makeMatrixNL dims
  (matP, elemVars) <- conPE matN "a" len
  (vecP, diag) <- conPE vecN "b" inner
  let cols = stripes inner elemVars
      colE scalar = fmap (infixApp' (varE _valueMul) scalar)
      colEs = zipWith colE diag cols
      resultE = appsE (conE matN : concat colEs)
      mulN = mkName $ "mul" ++ show left ++ "x" ++ show inner ++ "d"
      mulC = simpleClause [matP, vecP] resultE
      mulT = arrowsT [matT, vecT, matT]
      vecT = conT vecN
      matT = conT matN
  inlSigDef mulN mulT [mulC]

defineVectorOuterProduct :: ValueInfo -> (Int, Int) -> DecsQ
defineVectorOuterProduct ValueInfo{..} dims@(left, right) = do
  let vecN = makeVectorN left
      vecN' = makeVectorN right
      (matN, _) = makeMatrixNL dims
  (vecP, elemVars) <- conPE vecN "a" left
  (vecP', elemVars') <- conPE vecN' "b" right
  let elemEs = do
        x <- elemVars
        y <- elemVars'
        return $ infixApp' (varE _valueMul) x y
      resultE = appsE (conE matN : elemEs)
      mulN = mkName $ "mulT" ++ show left ++ "x" ++ show right
      mulC = simpleClause [vecP, vecP'] resultE
      mulT = arrowsT [vecT, vecT', matT]
      vecT = conT vecN
      vecT' = conT vecN'
      matT = conT matN
  inlSigDef mulN mulT [mulC]

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks chunkSize xs =
  let (front, back) = splitAt chunkSize xs in front:chunks chunkSize back

stripes :: Int -> [a] -> [[a]]
stripes chunkSize = raggedZip . chunks chunkSize

unevenZip :: Monoid a => [a] -> [a] -> [a]
unevenZip [] [] = []
unevenZip [] (x:xs) = x : unevenZip [] xs
unevenZip (x:xs) [] = x : unevenZip xs []
unevenZip (x:xs) (y:ys) = (x <> y) : unevenZip xs ys

raggedZip :: [[a]] -> [[a]]
raggedZip = foldr (unevenZip . fmap pure) []
