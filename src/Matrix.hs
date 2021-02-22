{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
             mysteryFunction,
             (-***-)
             ) where

import Types
import Data.List (transpose)
import qualified Data.Map as M


zipMatrix  :: (a -> b-> c) -> Matrix a -> Matrix b -> Matrix c
zipMatrix f (Matrix a) (Matrix b) = Matrix . zipWith (zipWith f) a $ b


--  Important Type Definition
data Matrix a = Matrix [[a]] deriving (Eq, Show, Functor)


instance (Num a) => VectorSpace (Matrix a) a where
  x -*- y = fmap (* x) y
  (-+-)  = (zipMatrix (+)) 


instance (Num a) => Algebra (Matrix a) a where
  (Matrix x) -**- (Matrix y)
    = Matrix . map f $ x
    where  f z =  map (foldl1 (+) . zipWith (*) z) y_t
           y_t = transpose y


type Coordinate  = (Int,Int)
type Matrix_size = (Int, Int)
type Kernel_size =  Matrix_size


-- Schor Product
(-***-) :: (Num a) =>  Matrix a -> Matrix a -> Matrix a
(-***-) =  (zipMatrix (*))


size :: Matrix a -> Matrix_size
size (Matrix []) = (0,0)
size (Matrix x)  = (length x, length . head $ x)


rowInc :: Int -> Coordinate -> Coordinate
rowInc x (r,c) = (r + x, c)

colInc :: Int -> Coordinate -> Coordinate
colInc x (r,c) = (r , c + x)


indexMatrix :: Matrix_size -> Matrix Coordinate
indexMatrix (r,c) =
  Matrix . zipWith (\rInc -> map (rowInc rInc)) rowIncrements $ vanillaMatrix
  where vanillaMatrix = take r . repeat .  map (0,) $ [0 .. (c - 1)]
        rowIncrements = [0 .. (r - 1)]


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
