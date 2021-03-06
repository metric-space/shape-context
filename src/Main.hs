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
import Canny


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


applyKernel :: Matrix Float -> Matrix Float -> Matrix Float
applyKernel filter matrix = fmap (\x -> ff x i filter subImageMap) im
  where subImageMap = mysteryFunction im (size filter)
        im = indexMatrix (size matrix)

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

--  Reader Monad info -> w,h image, pipeline tuple of 


main :: IO ()
main = do
         -- tests
         Env.getArgs
           >>= return . uncons
           >>= (\filename -> case filename of
                   Just (name,_) -> C.readImage name
                   Nothing -> return . Left $ "No filename given")
           >>= (\image -> case image@() of
                   Right i -> (
                     -- C.writePng "./output.png"
                     -- . C.pixelMap f28
                     let image@(Image w h _) = convert2Grey i
                         (Matrix b) = maximumSupression . gradientMatrix . (applyKernel gaussianFilter) $ image
                     in C.writePng "./output.png" (generateImage (\x y -> b !! x !! y) w h)
                     -- . show
                     -- . gradientMatrix
                     -- . convert2Grey $ i
                     )
                   Left msg -> putStrLn msg)
