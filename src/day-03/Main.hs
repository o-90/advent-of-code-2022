module Main where

import Data.List (splitAt)
import Control.Monad (sequence)
import qualified Data.Map as M

createDict :: String -> M.Map Char Int
createDict s = M.fromList $ zip s [1..]

processStr :: M.Map Char Int -> String -> Maybe Int
processStr m s = M.lookup (head k) m
  where
    k = M.keys $ M.intersection ld rd
    rd = createDict ri
    ld = createDict li
    (li, ri) = splitAt (length s `div` 2) s

main :: IO ()
main = do
  -- get input data
  inputs <- getContents
  -- map priority to items
  let priorityDict = createDict (['a'..'z'] ++ ['A'..'Z'])
  -- partially apply string processor with the priorities
  let processStrPriority = processStr priorityDict
  -- process strings
  let output = map processStrPriority (lines inputs)
  -- get sum
  print $ sum <$> sequence output
