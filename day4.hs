module Main where

import qualified Data.Vector as V
import Text.ParserCombinators.Parsec

type Matrix = V.Vector (V.Vector Int)
type Parsed = ([Int], [Matrix])

remainingBingoLine :: GenParser Char st [Int]
remainingBingoLine = (char ',' >> bingoLine) <|> (return [])

bingoLine :: GenParser Char st [Int]
bingoLine = do
  first <- bingoNumber
  next <- remainingBingoLine
  return (first : next)

bingoNumber :: GenParser Char st Int
bingoNumber = read <$> many $ noneOf ",\n"

--bingoFile :: GenParser Char st Parsed
bingoFile :: GenParser Char st [Int]
bingoFile = do
  header <- bingoLine
  return header
  --skipMany1 $ char '\n'

--parseInput :: String -> Either ParseError Parsed
parseInput :: String -> Either ParseError [Int]
parseInput input = parse bingoFile "" input

main :: IO ()
main = do
  textData <- readFile "day4.txt"
  putStrLn $ show $ parseInput textData
