{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Matrix (Matrix(..),
             Coordinate,
             Matrix_size,
             Kernel_size,
             size,
             rowInc,
             colInc,
             indexMatrix,
             matrixTraversal,
             pointToMatrix,
             mysteryFunction
             ) where

import qualified Data.Map as M


--  Important Type Definition
data Matrix a = Matrix [[a]] deriving (Eq, Show, Functor)

type Coordinate  = (Int,Int)
type Matrix_size = (Int, Int)
type Kernel_size =  Matrix_size


-- data Increment = R | D deriving (Eq, Show)


size :: Matrix a -> Matrix_size
size (Matrix []) = (0,0)
size (Matrix x)  = (length x, length . head $ x)


rowInc :: Int -> Coordinate -> Coordinate
rowInc x (r,c) = (r + x, c)

colInc :: Int -> Coordinate -> Coordinate
colInc x (r,c) = (r , c + x)


-- 
indexMatrix :: Matrix_size -> Matrix Coordinate
indexMatrix (r,c) =
  Matrix . zipWith (\rInc -> map (rowInc rInc)) rowIncrements $ vanillaMatrix
  where vanillaMatrix = take r . repeat .  map (0,) $ [0 .. (c - 1)]
        rowIncrements = [0 .. (r - 1)]


--rowMatrixInc :: Int -> Matrix (Int,Int) -> Matrix (Int,Int)
--rowMatrixInc inc = fmap (rowInc inc)
--
--
--index_matrix_inc :: [Matrix (Int, Int)] -> Increment -> [Matrix (Int,Int)]
--index_matrix_inc im R = im ++ [fmap (colInc 1) (last im)]
--index_matrix_inc im D = im ++ (map (fmap (rowInc 1)) im)
--
--
---- assume already validated before this step
--gen_index_matrix :: Kernel_size -> (Int,Int) -> [Matrix (Int, Int)]
--gen_index_matrix ks (r, d) = foldl index_matrix_inc [(indexMatrix ks)] ((replicate r R) ++ (replicate d D))
-- 
--
--break_matrix :: Matrix_size -> Kernel_size -> [Matrix (Int, Int)]
--break_matrix x@(mr,mc) y@(kr,kc) = let (rows,cols) = (mr - kr + 1, mc - kc + 1)
--                                   in if (rows*cols >=  0)
--                                      then (gen_index_matrix y (rows,cols))
--                                      else []


matrixTraversal :: Matrix a -> (Int,Int) -> [Coordinate]
matrixTraversal m offset@(x,y) =
  concat z
  where (mr,mc) = size m
        subMatrixSize = (mr - 2*x, mc - 2*y)
        (Matrix z) = fmap (colInc y . rowInc x) . indexMatrix $ subMatrixSize


translateTo :: Matrix Coordinate -> Coordinate -> Matrix Coordinate
translateTo m (i,j) =
  fmap (colInc j . rowInc i) m


-- this applies only when matrix is square
matrixMidCoords :: Matrix Coordinate -> Coordinate
matrixMidCoords matrix@(Matrix m) =
  (!! (mc `div` 2)) . (!! (mr `div` 2)) $ m
  where (mr,mc) = size matrix


-- given an index Point, generate a matrix of it's neighbours
pointToMatrix :: Coordinate -> Matrix_size -> Matrix Coordinate
pointToMatrix (r,c) ms@(mr,mc)
  = translateTo im (r-i, c-j)
  where im = indexMatrix ms
        (i,j) = matrixMidCoords im


mysteryFunction :: Matrix a -> Kernel_size -> M.Map Coordinate (Matrix Coordinate)
mysteryFunction m ks@(kr,kc) =
  M.fromList
  . map (\x -> (x, pointToMatrix x ks))
  . matrixTraversal m $ (div kr 2, div kc 2)
