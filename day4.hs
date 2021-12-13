--{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import qualified Data.Vector as V

import Data.List
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec hiding (many)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char (digit, oneOf, char)

import Control.Applicative ((<$>), (<*>), (<*), many, (<$), (<|>))
import Control.Monad (void)

newtype Matrix = Matrix (V.Vector (V.Vector Integer))
data Parsed = Parsed { bingoNumbers :: [Integer], bingoCards :: [Matrix] }

instance Show Parsed where
  show (Parsed {bingoNumbers=calledNumbers, bingoCards=cards}) = show calledNumbers ++ "\n" ++ (intercalate "\n\n" (map show cards))

instance Show Matrix where
  show (Matrix lines) = "----\n" ++ (intercalate "\n" listLines) ++ "\n----"
    where
      listLines = V.toList $ V.map (\line -> intercalate " " (listLine line)) lines
      listLine line = V.toList $ V.map show line

whitespace :: Parser ()
whitespace = void $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: Char -> Parser Char
symbol c = lexeme $ char c

integer :: Parser Integer
--integer = read <$> lexeme (many1 digit)
integer = read <$> many1 digit

commaSep p  = p `sepBy` (symbol ',')

bingoLine :: Parser (V.Vector Integer)
bingoLine = do
  nums <- integer `sepBy` (many (char ' '))
  return $ V.fromList nums

bingoCard :: Parser Matrix
bingoCard = do
  lines <- bingoLine `sepBy` (char '\n')
  whitespace
  return $ Matrix $ V.fromList lines

bingoFile :: Parser Parsed
bingoFile = do
  --header <- commaSep integer
  header <- integer `sepBy` (char ',')
  whitespace
  bingoCards <- bingoCard `sepBy` (char '\n')
  return $ Parsed {bingoNumbers=header, bingoCards=bingoCards}
  --skipMany1 $ char '\n'

parseInput :: String -> Either ParseError Parsed
parseInput input = parse bingoFile "day4.txt" input -- 2nd arg is just the filename to use in parseerror s

main :: IO ()
main = do
  textData <- readFile "day4.txt"
  putStrLn $ show $ parseInput textData
