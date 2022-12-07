module Main where

import Data.Foldable (foldl')

data Tree a = Node a [Tree a]
  deriving (Show)

data Info = Info {
    name   :: !String
  , parent :: !String
  , size   :: !Int
}

type FileTree = Tree Info

insert :: FileTree -> Info -> FileTree
insert (Node _ []) i = Node i []
insert (Node (Info rn rp rs) rt) (Info nn np ns)
  | rn /= np  = Node (Info rn rp rs       ) (map insert' rt              )
  | otherwise = Node (Info rn rp (rs + ns)) (Node (Info nn np ns) [] : rt)
  where
    insert' = flip insert $ Info nn np ns

fromList :: [Info] -> FileTree
fromList = foldl' insert (Node (Info "$ cd /" "None" 0) [])

parser :: String -> String
parser s = case words s of
  ["$", x, xs]
    | xs == "/" -> "root"
    | xs == ".." -> "stack.pop()"
    | otherwise -> "stack.push(" ++ xs ++ ")"
  ["$", xs] -> xs
  [x, xs]
    | x == "dir" -> xs
    | otherwise  -> x ++ " " ++ xs
  _ -> error "something is wrong!"

main :: IO ()
main = do
  inputs <- getContents
  print $ lines inputs
  print $ map parser $ lines inputs
