#!/usr/bin/env stack
{- stack script
 --resolver nightly-2019-12-22
 --package "vector"
 --ghc-options -Wall
-}
module Main where

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
    vertBitSelector' mat _ pos (zeroes, ones) selector | pos == (V.length mat) - 1 = selector zeroes ones
    vertBitSelector' mat bitPos pos (zeroes, ones) selector = if ((mat V.! pos) V.! bitPos) == 1 then vertBitSelector' mat bitPos (pos + 1) (zeroes, ones + 1) selector else vertBitSelector' mat bitPos (pos + 1) (zeroes + 1, ones) selector

mostCommonBit :: Matrix -> Int -> Int
mostCommonBit mat bitPos = vertBitSelector mat bitPos (\z o -> if z > o then 0 else 1)

leastCommonBit :: Matrix -> Int -> Int
--leastCommonBit mat bitPos = vertBitSelector mat bitPos (\z o -> if z > o then 1 else 0)
leastCommonBit mat bitPos = vertBitSelector mat bitPos selector
  where
    selector zeroes _ | zeroes == 0 = 1
    selector _ ones | ones == 0 = 0
    selector zeroes ones = if zeroes > ones then 1 else 0

findRating :: Matrix -> (Matrix -> Int -> Int) -> V.Vector Int
findRating mat_ fnc_ = findRating' mat_ fnc_ ((V.length (mat_ V.! 0)), 0)
  where
    findRating' mat _ _ | (V.length mat) == 1 = mat V.! 0
    --findRating' _ _ (len, pos) | pos >= len = error $ "ran out of iterations: " ++ pos -- TODO: fix
    findRating' mat fnc (len, pos) = let bitRequired = fnc mat pos in findRating' ((V.filter (filterByBit pos bitRequired)) mat) fnc (len, pos + 1)

oxygenRating :: Matrix -> V.Vector Int
oxygenRating mat = findRating mat mostCommonBit

co2ScrubberRating :: Matrix -> V.Vector Int
co2ScrubberRating mat = findRating mat leastCommonBit

vecToNumber :: V.Vector Int -> Int
vecToNumber v = getFirst $ V.foldr (\bit (sum_, exp_) -> (sum_ + bit * 2 ^ exp_, exp_ + 1)) ((0, 0) :: (Int, Int)) v
  where
    getFirst (sum_, _) = sum_

main :: IO ()
main = do
  textData <- readFile "data/day3.txt"
  let matrix = make2DVector $ filter (/= "") $ lines textData in
      let oxygenRatingNumber = vecToNumber $ oxygenRating $ matrix in
          let co2ScrubberRatingNumber = vecToNumber $ co2ScrubberRating $ matrix in putStrLn $ show $ oxygenRatingNumber * co2ScrubberRatingNumber
