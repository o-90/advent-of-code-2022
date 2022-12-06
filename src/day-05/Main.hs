module Main where

import qualified Data.Map as M
import           Data.Maybe (fromJust)

type Crate = [String]
type Cache = M.Map Int Crate

createDict :: [Crate] -> Cache
createDict c = M.fromList $ zip [1..] c

updateCrates :: Cache -> Int -> Int -> Int -> Cache
updateCrates m k f t = m''
  where
    m'' = M.insert t ocb m'
    m' = M.insert f oca m
    (oca, ocb) = (drop k ca, (reverse . take k $ ca) ++ cb)
    ca = fromJust $ M.lookup f m
    cb = fromJust $ M.lookup t m

updateCrates' :: Cache -> Int -> Int -> Int -> Cache
updateCrates' m k f t = m''
  where
    m'' = M.insert t ocb m'
    m' = M.insert f oca m
    (oca, ocb) = (drop k ca, take k ca ++ cb)
    ca = fromJust $ M.lookup f m
    cb = fromJust $ M.lookup t m

runner :: Cache -> [Int] -> Cache
runner c [x, y, z] = updateCrates c x y z

runner' :: Cache -> [Int] -> Cache
runner' c [x, y, z] = updateCrates' c x y z

getMultipleLines :: Int -> IO [String]
getMultipleLines n
  | n <= 0 = return []
  | otherwise = do
      x  <- getLine
      xs <- getMultipleLines (n - 1)
      let ret = x : xs
      return ret

main :: IO ()
main = do
  -- load stacks
  stacks <- getMultipleLines 9
  -- map crates to indices
  let crateMap = createDict $ map words stacks

  -- get instructions
  rawInstructions <- getContents
  let instructions = map (map (\ w -> read w :: Int) . words) . lines $ rawInstructions

  -- part one answer
  mapM_ (putStr . \ (_, y) -> head y) $ M.toList $ foldl runner crateMap instructions
  putStrLn ""

  -- part two answer
  mapM_ (putStr . \ (_, y) -> head y) $ M.toList $ foldl runner' crateMap instructions
  putStrLn ""
