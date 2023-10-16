import Bin
import Cmd
import Parser

import System.IO
import Control.Concurrent (threadDelay)

-- a small binary tree
a_tree :: Bin
a_tree = B (B L L) (B L L)

-- the top-level interactive loop
repl :: IO ()
repl = do
  putStrLn "Welcome to Binary Tree World.\n"
  putStrLn "You are at the root of an ancient binary tree."
  go (Hole,a_tree)
  where
    go :: BinZip -> IO ()
    go z = do                                          -- give the player some information
      case z of                                        -- about the current position in the tree
        (_,L)     -> putStrLn "You see a leaf."
        (_,B _ _) -> putStrLn "You see a binary node."
      putStr "> "                                      -- print the prompt
      hFlush stdout                                    -- flush standard output
      line <- getLine                                  -- get a line of input
      case parseInput parseCmd line of                 -- parse the input
          Nothing -> do
            putStrLn "I'm sorry, I do not understand."
            go z

          Just Go_Left ->
            case z of
              (c,B t1 t2) -> go (B0 c t2,t1)           -- climb up to the left
              (c,L) -> do
                putStrLn "You cannot climb any further."
                go z

          Just Go_Right ->
            case z of
              (c,B t1 t2) -> go (B1 t1 c,t2)           -- climb up to the right
              (c,L) -> do
                putStrLn "You cannot climb any further."
                go z

          Just Go_Down ->
            case z of
              (B0 c t2,t) -> go (c,B t t2)             -- climb down from the left, or
              (B1 t1 c,t) -> go (c,B t1 t)             -- climb down from the right, or
              (Hole,t) -> do                           -- already at the root
                putStrLn "You are already at the root."
                putStrLn "You cannot climb down any further."
                go z

          Just (Meditate n) -> do
            putStrLn "You close your eyes and focus on your surroundings."
            threadDelay (n * 1000000)
            putStrLn "You open your eyes."
            go z

          Just Quit -> do
            putStrLn "Okay."
            putStrLn "You ended the game over here:\n"
            putStrLn (drawBinZip z)
            putStrLn "Goodbye."
            return ()

main = repl
