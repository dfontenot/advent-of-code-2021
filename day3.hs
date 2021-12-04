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

leastCommon :: [Binary] -> Binary
leastCommon nums = leastCommon' (0, 0) nums
  where
    leastCommon' (zeroes, ones) [] = if ones > zeroes then Zero else One
    leastCommon' (zeroes, ones) (One:xs) = leastCommon' (zeroes, ones + 1) xs
    leastCommon' (zeroes, ones) (Zero:xs) = leastCommon' (zeroes + 1, ones) xs

-- TODO: cleanup
calculateBits :: ([Binary] -> Binary) -> [String] -> [Binary]
calculateBits fnc ((char:[]):xs) = (fnc (map (bitToBinary . (!! 0) . (take 1)) ((char:[]):xs))) : []
calculateBits fnc xs = (fnc (map (bitToBinary . (!! 0) . (take 1)) xs)) : (calculateBits fnc (map (drop 1) xs))

getGammaBits = calculateBits mostCommon
getEpsilonBits = calculateBits leastCommon

readBinaryBits :: [Binary] -> Int
readBinaryBits lst = readBinaryBits' (0, 0) $ reverseList lst
  where
    readBinaryBits' (sum, _) [] = sum
    readBinaryBits' (sum, exp) (x:xs) = readBinaryBits' (sum + (binaryToInt x) * 2 ^ exp, exp + 1) xs

main :: IO ()
main = do
  textData <- readFile "day3.txt"
  let lines_ = filter (/= "") $ lines textData in putStrLn $ show $ (readBinaryBits $ getGammaBits $ lines_) * (readBinaryBits $ getEpsilonBits $ lines_)
