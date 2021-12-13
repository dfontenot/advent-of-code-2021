module Main where

import qualified Data.Vector as V

import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec hiding (many)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char (digit, oneOf, char)

import Control.Applicative ((<$>), (<*>), (<*), many, (<$), (<|>))
import Control.Monad (void)

type Matrix = V.Vector (V.Vector Integer)
type Parsed = ([Integer], [Matrix])

-- remainingBingoLine :: GenParser Char st [Int]
-- remainingBingoLine = (char ',' >> bingoLine) <|> (return [])
--
-- bingoLine :: GenParser Char st [Int]
-- bingoLine = do
--   first <- bingoNumber
--   next <- remainingBingoLine
--   return (first : next)
--
-- bingoNumber :: GenParser Char st Int
-- bingoNumber = read <$> many $ noneOf ",\n"

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
  return $ V.fromList lines

bingoFile :: Parser Parsed
bingoFile = do
  --header <- commaSep integer
  header <- integer `sepBy` (char ',')
  whitespace
  bingoCards <- bingoCard `sepBy` (char '\n')
  return (header, bingoCards)
  --skipMany1 $ char '\n'

parseInput :: String -> Either ParseError Parsed
parseInput input = parse bingoFile "day4.txt" input -- 2nd arg is just the filename to use in parseerror s

main :: IO ()
main = do
  textData <- readFile "day4.txt"
  putStrLn $ show $ parseInput textData
