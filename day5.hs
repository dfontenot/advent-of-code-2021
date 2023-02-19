--{-# LANGUAGE XOverloadedStrings #-}
module Main where

import Prelude hiding (readFile, putStrLn)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Text.IO (readFile, putStrLn)
import Data.Void

type Parser = Parsec Void Text

main :: IO ()
main = do
  dataContents <- readFile "data/day5-test.txt"
  putStrLn dataContents
