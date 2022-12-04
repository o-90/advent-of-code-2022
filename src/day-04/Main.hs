module Main where

import Data.List.Split (splitOn)

checkPairs :: String -> Int
checkPairs s
  | la >= lb && ra <= rb = 1
  | la <= lb && ra >= rb = 1
  | otherwise            = 0
  where
    [la, ra] = map (\ w -> read w :: Int) a
    [lb, rb] = map (\ w -> read w :: Int) b
    [a,b] = map (splitOn "-") . splitOn "," $ s

checkPairs' :: String -> Int
checkPairs' s
  | la < rb && ra < lb = 0
  | la > rb && ra > lb = 0
  | otherwise          = 1
  where
    [la, ra] = map (\ w -> read w :: Int) a
    [lb, rb] = map (\ w -> read w :: Int) b
    [a,b] = map (splitOn "-") . splitOn "," $ s

main :: IO ()
main = do
  pairs <- getContents
  -- part one
  print $ sum $ map checkPairs (lines pairs)
  -- part two
  print $ sum $ map checkPairs' (lines pairs)
