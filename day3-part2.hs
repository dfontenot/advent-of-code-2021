#!/usr/bin/env stack
{- stack script
 --resolver nightly-2019-12-22
 --package "vector"
 --ghc-options -Wall
-}
module Day3 where

import qualified Data.Vector as V

make2DVector :: [String] -> V.Vector (V.Vector Int)
make2DVector lines = V.fromList $ map readLine lines
  where
    readLine line = V.fromList ((map (read . (\c -> [c])) line) :: [Int])

main :: IO ()
main = do
  textData <- readFile "day3.txt"
  putStrLn $ show $ make2DVector $ filter (/= "") $ lines textData
