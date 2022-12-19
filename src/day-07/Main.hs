module Main where

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

-- pull out the size of a node
extract :: RoseTree -> Int
extract (Node _ s _) = s

-- bottom-up update of the size of each node
sumTree :: RoseTree -> RoseTree
sumTree node@(Node _ _ []) = node
sumTree (Node l s t) = Node l s' t'
  where t' = fmap sumTree t
        s' = sum (map extract t') + s

-- find and return list of nodes that meet a criteria
filterTree :: (Int-> Bool) -> RoseTree -> [Int]
filterTree _ (Node _ _ []) = []
filterTree p (Node _ s t)
  | p s       = s : concatMap ftp t
  | otherwise = concatMap ftp t
  where ftp = filterTree p

-- push a node name to the stack
push :: String -> String -> String
push l r = l ++ " " ++ r

-- pop the stack when moving back a directory
pop :: String -> String
pop = unwords . tail . words

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
parser t s = go t s ""
  where
    go tree ("$ cd /" : xs) _ = go tree xs "/"
    go tree ("$ cd .." : xs) stack = go tree xs (pop stack)
    go tree (x : xs) stack = case words x of
      ["$", "cd", p] -> go tree xs (push p stack)
      ["$", "ls"]    -> go tree xs stack
      ["dir", p]     -> go (insert tree (stack, push p stack, 0)) xs stack
      [size, name]   -> go (insert tree (stack, push name stack, read size :: Int)) xs stack
      _              -> error "bork!"
    go tree [] _ = tree

main :: IO ()
main = do
  inputs <- getContents
  -- part one
  print $ sum . filterTree (<= 100000) . sumTree . parser rootSeed $ lines inputs
