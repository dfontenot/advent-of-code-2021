--{-# LANGUAGE XOverloadedStrings #-}
module Main where

import Prelude hiding (readFile)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, readFile)
import Data.Void

type Parser = Parsec Void Text

main :: IO ()
main = do
  dataContents <- readFile "data/day5-test.txt"
  putStrLn dataContents
