{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import qualified System.Environment as Env
import qualified Data.Map as M
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


ff :: Coordinate -> C.Image C.PixelF -> Matrix Float -> M.Map Coordinate (Matrix Coordinate) -> C.PixelF
ff c@(x,y) info kernel cmap =
  case (M.lookup c cmap) of (Just m) -> let (Matrix a) = kernel -***- fmap (\(x,y) -> C.pixelAt info x y) m
                                        in Prelude.foldl1 (+) . Prelude.concat $ a
                            Nothing -> C.pixelAt info x y


f :: C.Image C.PixelF -> IO ()
f i@(Image w h _) =
  C.writePng "./output.png" image
  where image = generateImage (\x y -> a !! x !! y) w h
        (Matrix a) = fmap (f28 . (\x -> ff x i edgeFilter mm))  im
        mm = mysteryFunction im (size edgeFilter)
        im = indexMatrix (w,h)

main :: IO ()
main = do
         k <- Env.getArgs >>= (C.readImage . Prelude.head)
         case k of (Right i) -> f (convert2Grey i)
                   _ -> putStrLn "Yeah .. things failed"
         return ()

