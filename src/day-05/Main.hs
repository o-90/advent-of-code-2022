module Main where

import qualified Data.Map as M
import           Data.Maybe (fromJust)

type Crate = [String]
type Policy = Crate -> Crate
type Cache = M.Map Int Crate

createDict :: [Crate] -> Cache
createDict c = M.fromList $ zip [1..] c

updateCrates :: Policy -> Cache -> Int -> Int -> Int -> Cache
updateCrates f m k l r = m''
  where
    m'' = M.insert r ocb m'
    m' = M.insert l oca m
    (oca, ocb) = (drop k ca, (f . take k $ ca) ++ cb)
    ca = fromJust $ M.lookup l m
    cb = fromJust $ M.lookup r m

runner :: Policy -> Cache -> [Int] -> Cache
runner f c [x, y, z] = updateCrates f c x y z

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
  mapM_ (putStr . \ (_, y) -> head y) $ M.toList $ foldl (runner reverse) crateMap instructions
  putStrLn ""

  -- part two answer
  mapM_ (putStr . \ (_, y) -> head y) $ M.toList $ foldl (runner id) crateMap instructions
  putStrLn ""
