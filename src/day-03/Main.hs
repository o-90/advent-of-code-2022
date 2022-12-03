module Main where

import           Control.Monad (sequence)
import           Data.List (splitAt)
import qualified Data.Map as M

type Cache = M.Map Char Int

createDict :: String -> Cache
createDict s = M.fromList $ zip s [1..]

processStr :: Cache -> String -> Maybe Int
processStr m s = M.lookup (head k) m
  where
    k = M.keys $ M.intersection ld rd
    [ld, rd] = map createDict [li, ri]
    (li, ri) = splitAt (length s `div` 2) s

processStrs :: Cache -> [String] -> Maybe Int
processStrs m s = M.lookup (head k) m
  where
    k = M.keys $ M.intersection m2 $ M.intersection m0 m1
    [m0, m1, m2] = map createDict s

makeGroups :: [String] -> [[String]]
makeGroups [] = []
makeGroups (x:y:z:zs) = [x, y, z] : makeGroups zs

main :: IO ()
main = do
  -- get input data
  inputs <- getContents
  -- map priority to items
  let priorityDict = createDict (['a'..'z'] ++ ['A'..'Z'])
  -- partially apply string processor with the priorities
  let processStrPriority = processStr priorityDict
  -- process strings
  let outputPartOne = map processStrPriority (lines inputs)
  -- get Part 1 sum
  print $ sum <$> sequence outputPartOne
  -- format data for Part 2
  let outputPartTwo = map (processStrs priorityDict) $ makeGroups (lines inputs)
  --get Part 2 sum
  print $ sum <$> sequence outputPartTwo
