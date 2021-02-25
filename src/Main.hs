{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import qualified System.Environment as Env
import qualified Data.Map as M
import Data.List
import Codec.Picture as C
import Codec.Picture.Types as T
import Data.Vector.Storable as V

import Matrix
import Types
import Test
import Image


-- promotePixel
gaussianFilter :: Matrix Float
gaussianFilter = (1.0/256.0) -*- Matrix [[1.0,4.0 ,6.0 ,4.0 ,1.0],
                                         [4.0,16.0,24.0,16.0,4.0],
                                         [6.0,24.0,36.0,24.0,6.0],
                                         [4.0,16.0,24.0,16.0,4.0],
                                         [1.0,4.0 ,6.0 ,4.0 ,1.0]]

edgeFilter :: Matrix Float
edgeFilter = Matrix [[-1.0,-1.0,-1.0],
                     [-1.0,8.0 ,-1.0],
                     [-1.0,-1.0,-1.0]]

-- name of this function
ff :: Coordinate -> C.Image C.PixelF -> Matrix Float -> M.Map Coordinate (Matrix Coordinate) -> C.PixelF
ff c@(x,y) info kernel cmap =
  case (M.lookup c cmap) of (Just m) -> let (Matrix a) = kernel -***- fmap (\(x,y) -> C.pixelAt info x y) m
                                        in Prelude.foldl1 (+) . Prelude.concat $ a
                            Nothing -> C.pixelAt info x y


applyKernel :: Matrix Float -> C.Image C.PixelF -> C.Image C.PixelF
applyKernel filter i@(Image w h _) =
  generateImage (\x y -> a !! x !! y) w h
  where (Matrix a) = fmap (\x -> ff x i filter subImageMap)  im
        subImageMap = mysteryFunction im (size filter)
        im = indexMatrix (w,h)

-- Reader monad based pipeline?

g_x :: Matrix Float
g_x = Matrix [[1.0, 0.0, -1.0],
              [2.0, 0.0, -2.0],
              [1.0, 0.0, -1.0]]


g_y :: Matrix Float
g_y = Matrix [[1.0, 2.0, 1.0],
              [0.0, 0.0, 0.0],
              [-1.0, -2.0, -1.0]]


sobelKernel :: C.Image C.PixelF -> C.Image C.PixelF
sobelKernel i@(Image w h _) =
 generateImage (\x y -> a !! x !! y) w h
  where (Matrix a) = zipMatrix (\x y -> (x**2 + y**2)**0.5) x y
        y = fmap (\x -> ff x i g_y subImageMap) im
        x = fmap (\x -> ff x i g_x subImageMap) im
        subImageMap = mysteryFunction im (size g_x)
        im = indexMatrix (w,h)


-- point of entry: C.Image C.PixelF ->  C.Image C.PixelF


main :: IO ()
main = do
         -- tests
         Env.getArgs
           >>= return . uncons
           >>= (\filename -> case filename of
                   Just (name,_) -> C.readImage name
                   Nothing -> return . Left $ "No filename given")
           >>= (\image -> case image of
                   Right i -> C.writePng "./output.png"
                     . C.pixelMap f28
                     . sobelKernel
                     . convert2Grey $ i
                   Left msg -> putStrLn msg)

