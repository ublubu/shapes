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

-- TODO: pull out common dot product code
defineMatrixMul :: ValueInfo -> (Int, Int, Int) -> DecsQ
defineMatrixMul vi@ValueInfo{..} (left, inner, right) = do
  let (matN, len) = makeMatrixNL vi (left, inner)
      (matN', len') = makeMatrixNL vi (inner, right)
      (matN'', _) = makeMatrixNL vi (left, right)
  (matP, elemVars) <- conPE matN "a" len
  (matP', elemVars') <- conPE matN "b" len'
  let rows = chunks inner elemVars
      cols = stripes right elemVars'
      dotE row col = foldl1 (infixApp' $ varE _valueAdd) products
        where products = uncurry (infixApp' $ varE _valueMul) <$> zip row col
      dotEs = do
        row <- rows
        col <- cols
        return $ dotE row col
      resultE = appsE (conE matN'' : dotEs)
      mulClause = clause [matP, matP'] (normalB resultE) []
      matT = conT matN
      matT' = conT matN'
      matT'' = conT matN''
      mulN = mkName $ "mul" ++ show left ++ "x" ++ show inner ++ "x" ++ show right
      mulT = arrowsT [matT, matT', matT'']
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
