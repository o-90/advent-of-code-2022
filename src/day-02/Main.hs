module Main where

import Data.List.Split (splitOn)

score :: String -> Int
score x
  | x == "A X" = 1 + 3
  | x == "A Y" = 2 + 6
  | x == "A Z" = 3 + 0
  | x == "B X" = 1 + 0
  | x == "B Y" = 2 + 3
  | x == "B Z" = 3 + 6
  | x == "C X" = 1 + 6
  | x == "C Y" = 2 + 0
  | x == "C Z" = 3 + 3
  | otherwise  = -1

score' :: String -> Int
score' x
  | x == "A X" = 3 + 0
  | x == "A Y" = 1 + 3
  | x == "A Z" = 2 + 6
  | x == "B X" = 1 + 0
  | x == "B Y" = 2 + 3
  | x == "B Z" = 3 + 6
  | x == "C X" = 2 + 0
  | x == "C Y" = 3 + 3
  | x == "C Z" = 1 + 6
  | otherwise  = -1

main :: IO ()
main = do
  plays <- getContents
  let vals = init $ splitOn "\n" plays
  print $ sum $ map score vals
  print $ sum $ map score' vals
