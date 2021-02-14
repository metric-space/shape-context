module Image (f28,
             convert2Grey) where

import Codec.Picture
import Codec.Picture.Types


rgb82F :: PixelRGB8 -> PixelF
rgb82F (PixelRGB8 a b c) = (/(255.0*3.0)) . sum . map (fromRational . toRational) $ [a , b , c]


f28 :: PixelF -> Pixel8
f28 = (floor . (255*) . max 0 . min 1)


convert2Grey :: DynamicImage -> Image PixelF
convert2Grey = pixelMap rgb82F . convertRGB8
