module Canny (gradientMatrix, ff, maximumSupression) where

import qualified Data.Map as M
import Data.List
import Codec.Picture as C
import Codec.Picture.Types as T
import Data.Vector.Storable as V

import Matrix
import Types
import Test
import Image


g_x :: Matrix Float
g_x = Matrix [[1.0, 0.0, -1.0],
              [2.0, 0.0, -2.0],
              [1.0, 0.0, -1.0]]


g_y :: Matrix Float
g_y = Matrix [[1.0, 2.0, 1.0],
              [0.0, 0.0, 0.0],
              [-1.0, -2.0, -1.0]]


type Angle = Float
type Magnitude = Float


ff :: Coordinate -> C.Image C.PixelF -> Matrix Float -> M.Map Coordinate (Matrix Coordinate) -> C.PixelF
ff c@(x,y) info kernel cmap =
  case (M.lookup c cmap) of (Just m) -> let (Matrix a) = kernel -***- fmap (\(x,y) -> C.pixelAt info x y) m
                                        in Prelude.foldl1 (+) . Prelude.concat $ a
                            Nothing -> C.pixelAt info x y


gradientMatrix :: C.Image C.PixelF -> Matrix (Magnitude, Angle)
gradientMatrix i@(Image w h _) =
  zipMatrix (\a b -> ((a**2 + b**2)**0.5, (atan (b/a))  * 180/pi)) x y
  where y = fmap (\x -> ff x i g_y subImageMap) im
        x = fmap (\x -> ff x i g_x subImageMap) im
        subImageMap = mysteryFunction im (size g_x)
        im = indexMatrix (w,h)


-- what do I need? The gradient matrix, the indexMap for each pixel
-- next steps round the second element in the tuple



customRounding :: Angle -> Angle
customRounding x
  | (0.0 <= x && x <= 22.5)     = 0.0
  | (22.5 <= x && x <= 45.0)    = 45.0
  | (45 <= x && x <= 67.5)      = 45.0
  | (67.5 < x) && (x <= 90.0)   = 90.0
  | (90 < x) && (x <= 112.5)    = 90.0
  | (112.5 < x) && (x <= 135.0) = 135.0
  | (135.0 < x) && (x <= 157.5) = 135.0
  | 157.0 < x  = 0.0


-- round the gradient angle according to formula here
step2 :: Matrix (Magnitude, Angle) -> Matrix (Magnitude, Angle)
step2 = fmap (\(x,y) -> (x, customRounding y))



hh :: Angle -> Matrix Coordinate -> [Coordinate]
hh x (Matrix a) = fmap (\(x,y) -> a !! x !! y ) k
  where k = case (abs x) of 0.0 -> [(1,0),(1,1)]
                            90.0 -> [(0,1),(2,1)]
                            135.0 -> [(0,0),(2,2)]
                            45.0 -> [(0,2),(2,0)]


-- TODO: CONTINUE HERE
step3Helper :: Coordinate -> Matrix (Magnitude, Angle) -> M.Map Coordinate (Matrix Coordinate) -> C.PixelF
step3Helper c@(x,y) (Matrix b) cmap =
  case (M.lookup c cmap) of (Just a@(Matrix m)) -> (if (m > (Prelude.foldl1 max d))
                                                     then m
                                                     else 0.0)
                                                   where d =  fmap (\(x,y) -> fst (b !! x !! y)) neighbours
                                                         neighbours = hh angle a
                                                         (m,angle) = b !! x !! y
                            Nothing -> fst (b !! x !! y)

-- maximum supression
 
-- step3 :: Matrix (Magnitude, Angle) ->  M.Map Coordinate (Matrix Coordinate) -> Matrix (Magnitude, Angle)
step3 :: Matrix (Magnitude, Angle) -> Matrix Magnitude
step3 matrix = fmap (\x -> step3Helper x matrix subImageMap) im
  where subImageMap = mysteryFunction im (3,3)
        im          = indexMatrix . size $ matrix


maximumSupression ::  Matrix (Magnitude, Angle) -> Matrix Magnitude
maximumSupression = step3 . step2
