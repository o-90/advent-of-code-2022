module Main where

import Data.Array
import Data.Char (digitToInt)

main :: IO ()
main = do
  inputs <- getContents
  let coords = lines inputs
  print [map digitToInt c | c <- coords]
