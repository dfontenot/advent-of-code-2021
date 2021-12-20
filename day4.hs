--{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import qualified Data.Vector as V

import Debug.Trace
import Data.List
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec hiding (many, State)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char (digit, oneOf, char)

import Control.Applicative (many)
import Control.Monad (void)
import Control.Monad.State

-- types to parse into
newtype Matrix a = Matrix (V.Vector (V.Vector a))
data Parsed = Parsed { bingoNumbers :: [Integer], bingoCards :: [Matrix Integer] }

-- gameplay types
data Cell = Cell { cellNum :: Integer, cellMarked :: Bool }
type Gamestate = [Matrix Cell]

instance Show Cell where
  show (Cell {cellNum=num, cellMarked=marked}) = "[" ++ (if marked then "(" ++ (show num) ++ ")" else (show num)) ++ "]"

instance Show Parsed where
  show (Parsed {bingoNumbers=calledNumbers, bingoCards=cards}) = show calledNumbers ++ "\n" ++ (intercalate "\n\n" (map show cards))

instance (Show a) => Show (Matrix a) where
  show (Matrix lines_) = "----\n" ++ (intercalate "\n" listLines) ++ "\n----"
    where
      listLines = V.toList $ V.map (\line -> intercalate " " (listLine line)) lines_
      listLine line = if (null line) then ["(empty line)"] else V.toList $ V.map show line

instance Foldable Matrix where
  foldr fnc acc (Matrix mat) = let flattened = V.foldr (V.++) V.empty mat in
                                   V.foldr fnc acc flattened

-- parsing stuff
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

-- running the bingo game stuff
initialGamestate :: Parsed -> Gamestate
initialGamestate (Parsed {bingoNumbers=_, bingoCards=cards}) = map matToCells cards
  where
    matToCells (Matrix mat) = Matrix $ V.map (\row -> V.map (\num -> Cell { cellNum=num, cellMarked=False }) row) mat

-- TODO: make use of (//) in vector to prevent unnecessary copying
markCellsWith :: (Integer -> Bool) -> Matrix Cell -> Matrix Cell
markCellsWith pred' (Matrix mat) = Matrix $ V.map (V.map updateCell) mat
  where
    updateCell (Cell {cellNum=num, cellMarked=marked}) = Cell {cellNum=num, cellMarked=marked || (pred' num)}

rotateMat :: Matrix a -> Matrix a
rotateMat (Matrix mat) = Matrix $ V.fromList $ collectMat 0 (V.length (mat V.! 0)) mat
  where
    collectMat i len mat' | i <= len - 1 = (V.map (\row -> row V.! i) mat'):(collectMat (i + 1) len mat')
    collectMat _ _ _ = []

checkRows :: Matrix Cell -> Bool
checkRows (Matrix x) = V.any (== True) (V.map (\row -> V.all cellMarked row) x)

winningCard :: Gamestate -> Maybe (Matrix Cell)
winningCard [] = Nothing
winningCard (x:xs) = if checkRows x || (checkRows . rotateMat) x then Just x else winningCard xs

filterWinningCards :: Gamestate -> Gamestate
filterWinningCards cards = filter (\x -> (not . checkRows) x && (not . checkRows . rotateMat) x) cards

unmarkedSum :: (Matrix Cell) -> Integer
unmarkedSum (Matrix mat) = V.sum $ V.map (\row -> V.sum (V.map (\c -> if cellMarked c then 0 else cellNum c) row)) mat

runBingoFirstWin :: [Integer] -> State Gamestate (Maybe Integer)
runBingoFirstWin [] = return Nothing
runBingoFirstWin (x:xs) = do
  cards <- get
  put $ map (markCellsWith ((==) x)) cards
  cards' <- get
  let scored = fmap ((*x) . unmarkedSum) (winningCard cards') in
      case scored of
        Nothing -> runBingoFirstWin xs
        _ -> return scored

runBingoLastWinner :: [Integer] -> State Gamestate (Maybe Integer)
runBingoLastWinner [] = return Nothing
runBingoLastWinner (x:xs) = do
  cards <- get
  put $ filterWinningCards $ map (markCellsWith ((==) x)) (trace "loop" cards)
  cards' <- get
  case cards' of
    [] -> return $ Just $ ((*x) . unmarkedSum) $ head (trace (show $ cards') cards)
    --(card:[]) -> return $ Just $ ((*x) . unmarkedSum) (trace "this one" card)
    _ -> runBingoLastWinner xs

main :: IO ()
main = do
  textData <- readFile "day4.txt"
  let parsed = parseInput textData in
      case parsed of
        --Right result -> case (evalState (runBingoFirstWin (bingoNumbers result)) (initialGamestate result)) of
        Right result -> case (evalState (runBingoLastWinner (bingoNumbers result)) (initialGamestate result)) of
                          Just answer -> print answer
                          Nothing -> putStrLn "no winner"
        Left err -> print err
