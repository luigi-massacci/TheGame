{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Game where

import Data.List
import Data.Set (fromList)
import GHC.Core.TyCon (newTyConEtadArity)
import Nodes
import Parser
import System.Random ()
import Types

-- pre-generated game tree structure
endNode :: a -> QuadTree a
endNode x = Node x Leaf Leaf Leaf Leaf

_HELP_MESSAGE :: String = "u need help? sad 4 u we don't have this here"

gameTree :: TreeZip TreeNode =
  TreeZip
    TOP
    ( Node
        helheim
        Leaf
        Leaf
        Leaf
        ( Node
            root
            (endNode muspelheim)
            Leaf
            Leaf
            ( Node
                midgard
                (endNode swartelheim)
                (endNode asgard)
                (endNode alvheim)
                Leaf
            )
        )
    )

-- Show children
previewTree :: QuadTree TreeNode -> [String]
previewTree Leaf = []
previewTree (Node lab _ _ _ _) = [previewmsg lab]

displayChildren :: QuadTree TreeNode -> [String]
displayChildren Leaf = []
displayChildren (Node a ll l r rr) = previewTree ll ++ previewTree l ++ previewTree r ++ previewTree rr

-- Gets user input
askAction :: IO Action
askAction = do
    putStr ">> "
    input <- getLine
    case parse input of
        Nothing -> do {putStrLn "Not a valid action"; askAction}
        Just a -> return a

-- Runs the main game loop
gameLoop :: GameInstance -> IO GameInstance
gameLoop g = do
  putStrLn (msg (label (tree (gamezip g))))
  case nodetype (label (tree (gamezip g))) of
    FightNode fightT defeatT life lifeName (Obj drop) ->
      if life > 0
        then do
          putStrLn fightT
          putStrLn ("It carries around a " ++ drop)
          putStrLn ("It seems to have " ++ show life ++ " " ++ lifeName ++ " left.")
        else putStrLn defeatT
    PlatformNode -> mapM_ putStrLn (displayChildren (tree (gamezip g)))
  action <- askAction
  case action of
    Help -> putStrLn ("\n" ++ _HELP_MESSAGE ++ "\n")
    _ -> putStrLn ""
  gameLoop (act action g)

-- we need smth like 'entry' which runs when we enter the node
runGame :: IO ()
runGame = do
  putStrLn "\nWelcome to Binary Tree World."
  gameLoop (act (Move "Root") (Game gameTree (Player 5 [])))
  return ()
