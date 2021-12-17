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
  void $ many $ char ' '
  nums <- integer `sepBy` (many1 (char ' '))
  return $ V.fromList nums

bingoCard' :: Parser Matrix
bingoCard' = do
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


bingoCard :: Parser [Matrix]
bingoCard = do
  lines_ <- (bingoLine `sepBy1` (char '\n')) `endBy` string "\n\n"
  --lines_ <- (bingoLine `endBy` string "\n\n") `sepBy1` char '\n'
  --lines_ :: () -- lines_ is [[V.Vector Integer]]
  -- -- TODO: there has to be a better way...
  -- line1 <- bingoLine `sepBy` char '\n'
  -- line2 <- bingoLine `sepBy` char '\n'
  -- line3 <- bingoLine `sepBy` char '\n'
  -- line4 <- bingoLine `sepBy` char '\n'
  -- line5 <- bingoLine `sepBy` char '\n'
  --_ <- many1 $ char '\n'
  --lines_ <- manyTill bingoLine (try (string "\n\n"))
  --lines_ <- bingoLine `sepBy` (char '\n')
  --whitespace
  --return $ Matrix (V.fromList lines_)
  --return $ Matrix [V.fromList [V.fromList [1,2]]]
  return $ map (\card -> Matrix $ V.fromList card) lines_
    -- where
    --   matrixify matrix = Matrix $ map V.fromList matrix

bingoFile :: Parser Parsed
bingoFile = do
  --header <- commaSep integer
  header <- integer `sepBy` (char ',')
  _ <- many1 $ char '\n'
  bingoCards_ <- bingoCard' `sepBy` (string "\n\n")
  --bingoCards_ <- bingoCard
  return $ Parsed {bingoNumbers=header, bingoCards=bingoCards_}
  --skipMany1 $ char '\n'

parseInput :: String -> Either ParseError Parsed
parseInput input = parse bingoFile "day4.txt" input -- 2nd arg is just the filename to use in parseerror s

main :: IO ()
main = do
  textData <- readFile "day4.txt"
  putStrLn $ show $ parseInput textData
