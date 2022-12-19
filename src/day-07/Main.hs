module Main where

import Data.Foldable (foldl')

data Tree a = Node {
    label :: a
  , size :: Int
  , subtree :: [Tree a]
} deriving (Show, Eq, Ord)

type RoseTree = Tree String
type Info = (String, String, Int)

-- empty node
rootSeed :: RoseTree
rootSeed = Node "/" 0 []

-- construct a rose tree
insert :: RoseTree -> Info -> RoseTree
insert (Node cl cs ct) (np, nl, ns)
  | cl /= np  = Node cl cs (map insert' ct)
  | otherwise = Node cl cs (Node nl ns [] : ct)
  where insert' = flip insert (np, nl, ns)

-- custom fold left to extract and reduce-sum the size of children
roseFoldl :: (a -> Int -> a) -> a -> [RoseTree] -> a
roseFoldl f z [] = z
roseFoldl f z (Node _ s _ : xs) = roseFoldl f (f z s) xs

-- bottom-up update the sizes of each node
sizes :: RoseTree -> RoseTree
sizes tree@(Node l s []) = tree
sizes (Node l s t) = Node l (s + s') t'
  where
    s' = roseFoldl (+) 0 t'
    t' = map sizes t

filterTree :: (Int -> Bool) -> RoseTree -> [Int]
filterTree _ (Node _ _ []) = []
filterTree p (Node _ s t)
  | p s       = s : concatMap ftp t
  | otherwise = concatMap ftp t
  where ftp = filterTree p

attach :: String -> Int -> String
attach s i = s ++ "_" ++ show i

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
parser t s = go t s [] 0
  where
    go tree ("$ cd /" : xs) [] 0 = go tree xs ["/"] 1
    go tree ("$ cd .." : xs) (_ : zs) n = go tree xs zs (n - 1)
    go tree (x : xs) stack@(z : zs) n = case words x of
      ["$", "cd", p] -> go tree xs (attach p n : stack) (n + 1)
      ["$", "ls"]    -> go tree xs stack n
      ["dir", p]     -> go (insert tree (z, attach p n, 0)) xs stack n
      [size, name]   -> go (insert tree (z, attach name n, read size :: Int)) xs stack n
      _              -> error "bork!"
    go tree _ [] _ = tree
    go tree [] _ _ = tree

main :: IO ()
main = do
  inputs <- getContents
  print $ sum $ filterTree (<= 100000) . sizes . parser rootSeed $ lines inputs
