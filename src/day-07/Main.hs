module Main where

data Tree a = Node {
    label :: a
  , size :: Int
  , subtree :: [Tree a]
} deriving (Show, Eq, Ord)

type RoseTree = Tree String
type Info = (String, String, Int)

insert :: RoseTree -> Info -> RoseTree
insert (Node cl cs ct) (np, nl, ns)
  | cl /= np  = Node cl cs (map insert' ct)
  | otherwise = Node cl cs (Node nl ns [] : ct)
  where insert' = flip insert (np, nl, ns)

rootSeed :: RoseTree
rootSeed = Node "/" 0 []

-- ---------------------------------------------------------------------------
-- -- parsing possibilitites
-- ---------------------------------------------------------------------------
-- $ cd /         -> push root to stack
-- $ cd ..        -> pop stack
-- $ cd [dir]     -> push dir to stack
-- $ ls           -> list items (move to next command)
-- dir [name]     -> create a node
-- [size] [name]  -> create a leaf
-- ---------------------------------------------------------------------------
parser :: RoseTree -> [String] -> RoseTree
parser t s = go t s []
  where
    go tree ("$ cd /" : xs) _ = go tree xs ["/"]
    go tree ("$ cd .." : xs) (_ : zs) = go tree xs zs
    go tree (x : xs) stack@(z : zs) = case words x of
      ["$", "cd", p] -> go tree xs (p : stack)
      ["$", "ls"]    -> go tree xs stack
      ["dir", p]     -> go (insert tree (z, p, 0)) xs stack
      [size, name]   -> go (insert tree (z, name, read size :: Int)) xs stack
      _              -> error "bork!"
    go tree _ [] = tree
    go tree [] _ = tree

main :: IO ()
main = do
  inputs <- getContents
  print $ parser rootSeed $ lines inputs
