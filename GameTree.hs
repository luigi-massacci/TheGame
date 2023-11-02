{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Game where

import Constants
import Data.List
import Data.Set (fromList)
import GHC.Core.TyCon (newTyConEtadArity)
import Nodes
import Parser
import System.Random ()
import Types

startWorld :: TreeZip TreeNode =
  TreeZip
    TOP
    ( Node
        helheim
        Leaf
        Leaf
        Leaf
        ( Node
            roots
            (mkTerminalNode muspelheim)
            (mkTerminalNode mimirs_lake)
            Leaf
            ( Node
                midgard
                (mkTerminalNode swartelheim)
                (mkTerminalNode asgard)
                (mkTerminalNode alvheim)
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
    Nothing -> do putStrLn "Not a valid action"; askAction
    Just a -> return a

displayMap :: GameInstance -> IO ()
displayMap = undefined

-- Runs the main game loop
gameLoop :: GameInstance -> IO GameInstance
gameLoop current_game = do
  putStrLn (description (label (tree (gamezip current_game))))
  case nodetype (label (tree (gamezip current_game))) of
    FightNode fightT defeatT life lifeName reward ->
      if life > 0
        then do
          putStrLn fightT
          putStrLn ("It carries around a " ++ reward)
          putStrLn ("It seems to have " ++ show life ++ " " ++ lifeName ++ " left.")
        else putStrLn defeatT
    PlatformNode -> mapM_ putStrLn (displayChildren (tree (gamezip current_game)))
  action <- askAction
  case action of
    Help -> putStrLn ("\n" ++ _HELP_MSG ++ "\n")
    ShowMap -> displayMap current_game
    _ -> putStrLn ""
  gameLoop (act action current_game)

-- we need smth like 'entry' which runs when we enter the node
runGame :: IO ()
runGame = do
  putStrLn "\nWelcome to Yggradasil, the World Tree."
  beginningGameInstance <- act (Move "Roots") (Game startWorld (Player 5 []))
  gameLoop beginningGameInstance
  return ()
