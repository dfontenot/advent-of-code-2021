#!/usr/bin/env stack
{- stack script
 --resolver nightly-2019-12-22
 --package "split"
 --ghc-options -Wall
-}

module Day2 where

import Data.List.Split

data Direction = Forward | Down | Up deriving (Show)
data Command = Command { direction :: Direction, count_ :: Int }

instance Read Direction where
  readsPrec _ input
    | input == "forward" = [(Forward, "")]
    | input == "down" = [(Down, "")]
    | input == "up" = [(Up, "")]

instance Read Command where
  readsPrec _ input = [(Command { direction = read (parts !! 0) :: Direction, count_ = read (parts !! 1) :: Int }, "")]
    where parts = splitOn " " input

instance Show Command where
  show (Command {direction=d, count_=cnt}) = (show d) ++ " " ++ (show cnt)

processCommands :: [Command] -> (Int, Int)
processCommands commands = processCommands' (0, 0) commands
  where
    processCommands' pos [] = pos
    processCommands' (x, y) ((Command {direction=Forward, count_=cnt}):rst) = processCommands' (x + cnt, y) rst
    processCommands' (x, y) ((Command {direction=Down, count_=cnt}):rst) = processCommands' (x, y + cnt) rst
    processCommands' (x, y) ((Command {direction=Up, count_=cnt}):rst) = processCommands' (x, y - cnt) rst

answer :: (Int, Int) -> Int
answer (x, y) = x * y

main :: IO ()
main = do
  textData <- readFile "day2.txt"
  putStrLn $ show $ answer $ processCommands $ map (read :: String -> Command) $ filter (/= "") $ lines textData
