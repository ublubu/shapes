{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

module Shapes.Linear.MatrixTemplate where

import Control.Monad
import Data.Monoid
import Language.Haskell.TH

import Shapes.Linear.Template

makeMatrixNL :: ValueInfo -> (Int, Int) -> (Name, Int)
makeMatrixNL vi@ValueInfo{..} (rows, cols) =
  (mkName $ "M" ++ show rows ++ "x" ++ show cols, rows * cols)

makeMatrixType :: ValueInfo -> (Int, Int) -> DecsQ
makeMatrixType vi@ValueInfo{..} dims@(rows, cols) = do
  let (matrixN, len) = makeMatrixNL vi dims
      constrArg = strictType notStrict (conT _valueN)
      definers = [ defineLift
                 , defineLift2
                 , defineFromList
                 , defineToList
                 , deriveShow
                 , deriveArbitrary
                 ]
  impls <- concat <$> mapM (\f -> f matrixN vi len) definers
  matrixD <- dataD (cxt []) matrixN [] [normalC matrixN (replicate len constrArg)] []
  return $ matrixD : impls

defineMatrixMul :: ValueInfo -> (Int, Int, Int) -> DecsQ
defineMatrixMul vi@ValueInfo{..} (left, inner, right) = do
  let (matN, len) = makeMatrixNL vi (left, inner)
      (matN', len') = makeMatrixNL vi (inner, right)
      (matN'', _) = makeMatrixNL vi (left, right)
  (matP, elemVars) <- conPE matN "a" len
  (matP', elemVars') <- conPE matN "b" len'
  let rows = chunks inner elemVars
      cols = stripes right elemVars'
      dotEs = do
        row <- rows
        col <- cols
        return $ dotE vi row col
      resultE = appsE (conE matN'' : dotEs)
      mulClause = clause [matP, matP'] (normalB resultE) []
      matT = conT matN
      matT' = conT matN'
      matT'' = conT matN''
      mulN = mkName $ "mul" ++ show left ++ "x" ++ show inner ++ "x" ++ show right
      mulT = arrowsT [matT, matT', matT'']
  funSigDef mulN mulT [mulClause]

defineMatrixMulVector :: ValueInfo -> (Int, Int) -> DecsQ
defineMatrixMulVector vi@ValueInfo{..} dims@(left, inner) = do
  let (matN, len) = makeMatrixNL vi dims
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
      mulClause = clause [matP, vecP] (normalB resultE) []
      matT = conT matN
      vecT = conT vecN
      vecT' = conT vecN'
      mulT = arrowsT [matT, vecT, vecT']
  funSigDef mulN mulT [mulClause]

defineVectorMulMatrix :: ValueInfo -> (Int, Int) -> DecsQ
defineVectorMulMatrix vi@ValueInfo{..} dims@(inner, right) = do
  let vecN = makeVectorN inner
      (matN, len) = makeMatrixNL vi dims
      vecN' = makeVectorN right
  (vecP, row) <- conPE vecN "b" inner
  (matP, elemVars) <- conPE matN "a" len
  let cols = stripes right elemVars
      dotEs = do
        col <- cols
        return $ dotE vi row col
      resultE = appsE (conE vecN' : dotEs)
      mulN = mkName $ "mulr" ++ show inner ++ "x" ++ show right
      mulClause = clause [vecP, matP] (normalB resultE) []
      vecT = conT vecN
      matT = conT matN
      vecT' = conT vecN'
      mulT = arrowsT [vecT, matT, vecT']
  funSigDef mulN mulT [mulClause]

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
