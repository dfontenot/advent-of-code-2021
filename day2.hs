#!/usr/bin/env stack
{- stack script
 --resolver nightly-2019-12-22
 --package "split text"
 --ghc-options -Wall
-}
{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.List.Split
import Data.Text hiding (splitOn, lines)

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

main :: IO ()
main = do
  textData <- readFile "day2.txt"
  putStrLn $ show $ (read ((lines textData) !! 0) :: Command)
  --putStrLn $ show $ (lines textData) !! 0
