module Day3 where

data Binary = One | Zero deriving (Show)

reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

bitToBinary :: Char -> Binary
bitToBinary '0' = Zero
bitToBinary '1' = One
bitToBinary _ = error "invalid data"

binaryToInt :: Binary -> Int
binaryToInt Zero = 0
binaryToInt One = 1

mostCommon :: [Binary] -> Binary
mostCommon nums = mostCommon' (0, 0) nums
  where
    mostCommon' (zeroes, ones) [] = if ones > zeroes then One else Zero
    mostCommon' (zeroes, ones) (One:xs) = mostCommon' (zeroes, ones + 1) xs
    mostCommon' (zeroes, ones) (Zero:xs) = mostCommon' (zeroes + 1, ones) xs

-- TODO: cleanup
getGammaBits :: [String] -> [Binary]
getGammaBits ((char:[]):xs) = (mostCommon (map (bitToBinary . (!! 0) . (take 1)) ((char:[]):xs))) : []
getGammaBits xs = (mostCommon (map (bitToBinary . (!! 0) . (take 1)) xs)) : (getGammaBits (map (drop 1) xs))

readBinaryBits :: [Binary] -> Int
readBinaryBits lst = readBinaryBits' (0, 0) $ reverseList lst
  where
    readBinaryBits' (sum, _) [] = sum
    readBinaryBits' (sum, exp) (x:xs) = readBinaryBits' (sum + (binaryToInt x) * 2 ^ exp, exp + 1) xs

main :: IO ()
main = do
  textData <- readFile "day3.txt"
  putStrLn $ show $ readBinaryBits $ getGammaBits $ filter (/= "") $ lines textData
