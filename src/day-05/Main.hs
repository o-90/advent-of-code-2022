module Main where

import           Control.Monad (foldM)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           Data.Stack (Stack, stackNew, stackPop, stackPush)

type Crate = Stack String
type Cache = M.Map Int Crate

loadCrate :: [String] -> Crate
loadCrate = foldl stackPush stackNew

createDict :: [Crate] -> Cache
createDict c = M.fromList $ zip [1..] c

popAndPush :: (Crate, Crate) -> (Crate, Crate)
popAndPush (ca, cb) = case stackPop ca of
  Just (nStack, nItem) -> (nStack, stackPush cb nItem)
  _                    -> (ca, cb)

updateCrates :: Cache -> Int -> Int -> Int -> Cache
updateCrates m k f t = m''
  where
    m'' = M.insert t ocb m'
    m' = M.insert f oca m
    (oca, ocb) = iterate popAndPush (ca, cb) !! k
    ca = fromJust $ M.lookup f m
    cb = fromJust $ M.lookup t m

runner :: Cache -> [Int] -> Cache
runner c [x, y, z] = updateCrates c x y z

main :: IO ()
main = do
  -- load stacks
  input1 <- getLine
  input2 <- getLine
  input3 <- getLine
  input4 <- getLine
  input5 <- getLine
  input6 <- getLine
  input7 <- getLine
  input8 <- getLine
  input9 <- getLine

  -- TODO(o-90): this could be better
  let crateMap = createDict $ map (loadCrate . words) [input1, input2, input3, input4, input5, input6, input7, input8, input9]

  -- get instructions
  rawInstructions <- getContents
  let instructions = map (map(\w -> read w :: Int) . words) . lines $ rawInstructions

  -- part one answer
  print $ foldl runner crateMap instructions
