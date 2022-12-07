module Main where

data Tree a = Empty | Node a [Tree a]
  deriving (Show)

data Info = Info {
    name   :: !String
  , parent :: !String
  , size   :: !Int
} deriving (Show)

type FileTree = Tree Info

insert :: FileTree -> Info -> FileTree
insert Empty i = Node i []
insert (Node (Info rn rp rs) rt) (Info nn np ns)
  | rn == np  = Node (Info rn rp (rs + ns)) (Node (Info nn np ns) [] : rt)
  | otherwise = Node (Info rn rp rs       ) (map insert' rt              )
  where
    insert' = flip insert $ Info nn np ns

foldc :: FileTree -> [String] -> FileTree
foldc t []       = t
foldc t (y : ys) = case words y of
  ["$", x, xs]
    | xs == "/"  -> foldc (insert t (Info "cd /" "None" 0)) ys
    | xs == ".." -> foldc t ys
    | otherwise  -> foldc t ys
  ["$", xs]      -> foldc t ys
  [x, xs]
    | x == "dir" -> foldc t ys
    | otherwise  -> foldc (insert t (Info xs "cd /" (read x :: Int))) ys
  _              -> foldc t ys

main :: IO ()
main = do
  inputs <- getContents
  print $ foldc Empty $ lines inputs
