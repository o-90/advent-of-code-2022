module Main where

import Data.List.Split (splitOn)

checkPairs :: String -> Int
checkPairs s
  | la >= lb && ra <= rb = 1
  | la <= lb && ra >= rb = 1
  | otherwise            = 0
  where
    [la, ra] = map (\ w -> read w :: Int) l
    [lb, rb] = map (\ w -> read w :: Int) r
    [l,r] = map (splitOn "-") . splitOn "," $ s

main :: IO ()
main = do
  pairs <- getContents
  let example = head $ lines pairs
  print $ sum $ map checkPairs (lines pairs)
