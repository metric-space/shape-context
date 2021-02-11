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


-- promotePixel
gaussianFilter :: Matrix Float
gaussianFilter = (1.0/256.0) -*- Matrix [[1.0,4.0 ,6.0 ,4.0 ,1.0],
                                         [4.0,16.0,24.0,16.0,4.0],
                                         [6.0,24.0,36.0,24.0,6.0],
                                         [4.0,16.0,24.0,16.0,4.0],
                                         [1.0,4.0 ,6.0 ,4.0 ,1.0]]

edgeFilter :: Matrix Float
edgeFilter = Matrix [[-1.0,-1.0,-1.0],[-1.0,8.0,-1.0],[-1.0,-1.0,-1.0]]

instance Num C.PixelRGBF where
 (C.PixelRGBF a b c) +  (C.PixelRGBF d e f) = (C.PixelRGBF (a + d) (b + e) (c + f))
 (C.PixelRGBF a b c) *  (C.PixelRGBF d e f) = (C.PixelRGBF (a * d) (b * e) (c * f))


m1 :: C.PixelRGBF -> (Float, Float, Float)
m1 (C.PixelRGBF a b c) = (a,b,c)

m2 :: Float -> C.PixelRGBF
m2 a = (C.PixelRGBF a a a)

cf :: C.PixelRGBF -> C.PixelRGB8
cf (C.PixelRGBF a b c) = C.PixelRGB8 (f a) (f b) (f c)
                         where f = (floor . (255*) . max 0 . min 1)

ff :: Coordinate -> C.Image C.PixelRGB8 -> Matrix Float -> M.Map Coordinate (Matrix Coordinate) -> C.PixelRGB8
ff c@(x,y) info kernel cmap =
  case (M.lookup c cmap) of (Just m) -> let (Matrix a) = (fmap m2 kernel) -***- fmap (\(x,y) -> T.promotePixel $ ( C.pixelAt info x y)) m
                                        in  cf . Prelude.foldl1 (+) . Prelude.concat $ a
                            Nothing -> C.pixelAt info x y

f :: C.Image C.PixelRGB8 -> IO ()
f i@(Image w h _) =
  C.writePng "./output.png" image
  where image = generateImage (\x y -> a !! x !! y) w h
        (Matrix a) = fmap (\x -> ff x i edgeFilter mm) im
        mm = mysteryFunction im (size edgeFilter)
        im = indexMatrix (w,h)

main :: IO ()
main = do
         k <- Env.getArgs >>= (C.readImage . Prelude.head)
         case k of (Right i) -> f (convertRGB8 i)
                   _ -> putStrLn "Yeah .. things failed"
         return ()

