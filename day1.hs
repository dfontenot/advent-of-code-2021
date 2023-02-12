get2nd (_, a) = a

acc :: (Maybe Int, Int) -> Int -> (Maybe Int, Int)
acc (Nothing, _) curDepth = (Just curDepth, 0)
acc (Just depth, count) curDepth = (Just curDepth, if curDepth > depth then count + 1 else count)

main :: IO ()
main = do
  textData <- readFile "data/day1.txt"
  putStrLn $ show $ get2nd $ foldl acc (Nothing, 0) $ map (read :: String -> Int) $ filter (/= "") $ lines textData
