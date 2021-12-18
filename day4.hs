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

-- types to parse into
newtype Matrix a = Matrix (V.Vector (V.Vector a))
data Parsed = Parsed { bingoNumbers :: [Integer], bingoCards :: [Matrix Integer] }

-- gameplay types
data Cell = Cell { cellNum :: Integer, cellMarked :: Bool }
data Gamestate = Gamestate { bingoNums :: [Integer], bingoCardz :: [Matrix Cell] }

instance Show Gamestate where
  show (Gamestate {bingoNums=nums, bingoCardz=cards}) = show ("nums: " ++ (show nums)) ++ "\n" ++ (intercalate "\n" (map show cards))

instance Show Cell where
  show (Cell {cellNum=num, cellMarked=marked}) = "[" ++ (if marked then "(" ++ (show num) ++ ")" else (show num)) ++ "]"

instance Show Parsed where
  show (Parsed {bingoNumbers=calledNumbers, bingoCards=cards}) = show calledNumbers ++ "\n" ++ (intercalate "\n\n" (map show cards))

instance (Show a) => Show (Matrix a) where
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

bingoCard :: Parser (Matrix Integer)
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

bingoCards' :: Parser [Matrix Integer]
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

initialGamestate :: Parsed -> Gamestate
initialGamestate (Parsed {bingoNumbers=calledNumbers, bingoCards=cards}) = Gamestate {bingoNums=calledNumbers, bingoCardz=map matToCells cards}
  where
    matToCells (Matrix mat) = Matrix $ V.map (\row -> V.map (\num -> Cell { cellNum=num, cellMarked=False }) row) mat

main :: IO ()
main = do
  textData <- readFile "day4.txt"
  let parsed = parseInput textData in
      case parsed of
        Right result -> putStrLn $ show $ initialGamestate result
        Left err -> putStrLn $ show $ err
