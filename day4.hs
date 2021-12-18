--{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import qualified Data.Vector as V

import Data.List
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec hiding (many)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char (digit, oneOf, char)

import Control.Applicative (many)
import Control.Monad (void)

newtype Matrix = Matrix (V.Vector (V.Vector Integer))
data Parsed = Parsed { bingoNumbers :: [Integer], bingoCards :: [Matrix] }

instance Show Parsed where
  show (Parsed {bingoNumbers=calledNumbers, bingoCards=cards}) = show calledNumbers ++ "\n" ++ (intercalate "\n\n" (map show cards))

instance Show Matrix where
  show (Matrix lines_) = "----\n" ++ (intercalate "\n" listLines) ++ "\n----"
    where
      listLines = V.toList $ V.map (\line -> intercalate " " (listLine line)) lines_
      listLine line = if (null line) then ["(empty line)"] else V.toList $ V.map show line

integer :: Parser Integer
integer = read <$> many1 digit

bingoLine :: Parser (V.Vector Integer)
bingoLine = do
  void $ many $ char ' '
  nums <- integer `sepBy` (many1 (char ' '))
  return $ V.fromList nums

bingoCard :: Parser Matrix
bingoCard = do
  line1 <- bingoLine
  void $ char '\n'
  line2 <- bingoLine
  void $ char '\n'
  line3 <- bingoLine
  void $ char '\n'
  line4 <- bingoLine
  void $ char '\n'
  line5 <- bingoLine
  return $ Matrix $ V.fromList [line1, line2, line3, line4, line5]

bingoCards' :: Parser [Matrix]
bingoCards' = do
  mat <- bingoCard
  rstMat <- rstBingoCards
  return (mat:rstMat)
    where
      rstBingoCards = try ((many1 (char '\n')) >> bingoCards') <|> (return [])

bingoFile :: Parser Parsed
bingoFile = do
  header <- integer `sepBy` (char ',')
  _ <- many1 $ char '\n'
  bingoCards_ <- bingoCards'
  skipMany $ char '\n'
  eof
  return $ Parsed {bingoNumbers=header, bingoCards=bingoCards_}

parseInput :: String -> Either ParseError Parsed
parseInput input = parse bingoFile "day4.txt" input -- 2nd arg is just the filename to use in parseerror s

main :: IO ()
main = do
  textData <- readFile "day4.txt"
  putStrLn $ show $ parseInput textData
