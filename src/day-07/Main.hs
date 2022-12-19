module Main where

data Tree a = Node {
    label :: a
  , size :: Int
  , subtree :: [Tree a]
} deriving (Show, Eq, Ord)

type RoseTree = Tree String
type Info = (String, String, Int)

rootSeed :: RoseTree
rootSeed = Node "/" 0 []

insert :: RoseTree -> Info -> RoseTree
insert (Node cl cs ct) (np, nl, ns)
  | cl /= np  = Node cl cs (map insert' ct)
  | otherwise = Node cl cs (Node nl ns [] : ct)
  where insert' = flip insert (np, nl, ns)

extract :: RoseTree -> Int
extract (Node _ s _) = s

extractRoot :: RoseTree -> Int
extractRoot (Node l s _) = if l == "/" then s else 0

sumTree :: RoseTree -> RoseTree
sumTree node@(Node _ _ []) = node
sumTree (Node l s t) = Node l s' t'
  where t' = fmap sumTree t
        s' = sum (map extract t') + s

filterTree :: (Int-> Bool) -> RoseTree -> [Int]
filterTree _ (Node _ _ []) = []
filterTree p (Node _ s t)
  | p s       = s : concatMap ftp t
  | otherwise = concatMap ftp t
  where ftp = filterTree p

push :: String -> String -> String
push l r = l ++ " " ++ r

pop :: String -> String
pop = unwords . tail . words

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
  let tree = sumTree . parser rootSeed $ lines inputs
  -- part one
  print $ sum . filterTree (<= 100000) $ tree
  -- part two
  print $ minimum . filterTree (>= 30000000 - (70000000 - extractRoot tree)) $ tree
