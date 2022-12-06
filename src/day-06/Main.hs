module Main where

import Data.List (nub)

isUniqueSameAsFull :: (Eq a) => Int -> [a] -> Bool
isUniqueSameAsFull k xs = k == length ((nub . take k) xs)

runner :: Int -> String -> Int
runner _ [] = 0
runner k l@(_:xs)
  | isUniqueSameAsFull k l = k
  | otherwise              = 1 + runner k xs

main :: IO ()
main = do
  args <- getLine
  -- part one
  print $ runner 4 args
  -- part two
  print $ runner 14 args
