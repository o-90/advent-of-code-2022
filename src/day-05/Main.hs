module Main where

main :: IO ()
main = do
  -- load stacks
  stack1 <- getLine
  stack2 <- getLine
  stack3 <- getLine
  stack4 <- getLine
  stack5 <- getLine
  stack6 <- getLine
  stack7 <- getLine
  stack8 <- getLine
  stack9 <- getLine

  -- audit
  print $ words stack1
  print $ words stack2
  print $ words stack3
  print $ words stack4
  print $ words stack5
  print $ words stack6
  print $ words stack7
  print $ words stack8
  print $ words stack9

  -- load stack instructions
  instructions <- getContents

  -- audit
  print $ lines instructions
