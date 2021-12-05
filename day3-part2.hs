#!/usr/bin/env stack
{- stack script
 --resolver nightly-2019-12-22
 --package "vector"
 --ghc-options -Wall
-}
module Day3 where

import qualified Data.Vector as V

type Matrix = V.Vector (V.Vector Int)

make2DVector :: [String] -> Matrix
make2DVector lines_ = V.fromList $ map readLine lines_
  where
    readLine line = V.fromList ((map (read . (\c -> [c])) line) :: [Int])

filterByBit :: Int -> Int -> V.Vector Int -> Bool
filterByBit bitPos bitRequired vec = (vec V.! bitPos) == bitRequired

-- TODO: cleanup
vertBitSelector :: Matrix -> Int -> (Int -> Int -> Int) -> Int
vertBitSelector mat_ bitPos_ selector_ = vertBitSelector' mat_ bitPos_ 0 (0, 0) selector_
  where
    vertBitSelector' mat _ pos (zeroes, ones) selector | pos == (length mat) - 1 = selector zeroes ones
    vertBitSelector' mat bitPos pos (zeroes, ones) selector = if ((mat V.! pos) V.! bitPos) == 1 then vertBitSelector' mat bitPos (pos + 1) (zeroes, ones + 1) selector else vertBitSelector' mat bitPos (pos + 1) (zeroes + 1, ones) selector

mostCommonBit :: Matrix -> Int -> Int
mostCommonBit mat bitPos = vertBitSelector mat bitPos (\z o -> if z > o then 0 else 1)

leastCommonBit :: Matrix -> Int -> Int
leastCommonBit mat bitPos = vertBitSelector mat bitPos (\z o -> if z > o then 1 else 0)

oxygenRating :: Matrix -> V.Vector Int
oxygenRating mat_ = oxygenRating' mat_ ((V.length (mat_ V.! 0)), 0)
  where
    oxygenRating' mat _ | (V.length mat) == 1 = mat V.! 0
    --oxygenRating' _ (len, pos) | pos >= len = error $ "ran out of iterations: " ++ pos
    oxygenRating' mat (len, pos) = let bitRequired = mostCommonBit mat pos in oxygenRating' ((V.filter (filterByBit pos bitRequired)) mat) (len, pos + 1)

main :: IO ()
main = do
  textData <- readFile "day3.txt"
  --putStrLn $ show $ make2DVector $ filter (/= "") $ lines textData
  putStrLn $ show $ oxygenRating $ make2DVector $ filter (/= "") $ lines textData
