module Main where

import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  calories <- getContents
  let cals = init $ splitOn "\n\n" calories
  let ans = map (sum . map (\ w -> read w :: Int) . splitOn "\n") cals
  print $ maximum ans
  print $ sum $ take 3 $ (reverse. sort) ans
