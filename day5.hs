--{-# LANGUAGE XOverloadedStrings #-}
module Main where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text.IO (readFile)
import Data.Void
import Prelude hiding (readFile)
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

type Coord = (Int, Int)
type Line = (Coord, Coord)

type Parser = Parsec Void Text

lexeme = L.lexeme space
symbol = L.symbol space

integer :: Parser Int
integer = lexeme L.decimal

parseCoord :: Parser Coord
parseCoord = do
  x <- dbg "int" integer
  void (symbol "," <?> "digit separator")
  y <- dbg "int" integer
  return (x, y)

parseLine :: Parser Line
parseLine = do
  start <- dbg "x coord" parseCoord <?> "start coordinate"
  void (symbol "->" <?> "coordinate separator")
  end <- dbg "y coord" parseCoord <?> "end coordinate"
  return (start, end)

parseFile :: Parser [Line]
parseFile = parseLine `endBy1` newline

main :: IO ()
main = do
  dataContents <- readFile "data/day5-test.txt"
  case runParser parseFile "(day5 input)" dataContents of
    Left err -> putStrLn $ errorBundlePretty err
    Right result -> print result
