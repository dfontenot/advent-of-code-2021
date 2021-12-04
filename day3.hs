module Day3 where

data Binary = One | Zero deriving (Show)

bitToBinary :: Char -> Binary
bitToBinary '0' = Zero
bitToBinary '1' = One
bitToBinary _ = error "invalid data"

mostCommon :: [Binary] -> Binary
mostCommon nums = mostCommon' (0, 0) nums
  where
    mostCommon' (zeroes, ones) [] = if ones > zeroes then One else Zero
    mostCommon' (zeroes, ones) (One:xs) = mostCommon' (zeroes, ones + 1) xs
    mostCommon' (zeroes, ones) (Zero:xs) = mostCommon' (zeroes + 1, ones) xs

-- TODO: cleanup
findGamma :: [String] -> [Binary]
findGamma ((char:[]):xs) = (mostCommon (map (bitToBinary . (!! 0) . (take 1)) ((char:[]):xs))) : []
findGamma xs = (mostCommon (map (bitToBinary . (!! 0) . (take 1)) xs)) : (findGamma (map (drop 1) xs))

main :: IO ()
main = do
  textData <- readFile "day3.txt"
  putStrLn $ show $ findGamma $ filter (/= "") $ lines textData
