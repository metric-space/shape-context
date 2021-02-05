module Main (main) where

import qualified System.Environment as Env
import qualified Data.Map as M
import Codec.Picture as C
import Matrix
import Test

main :: IO ()
main = do
         -- pic <- Env.getArgs >>= ( C.readImage . head)
         tests
         return ()

