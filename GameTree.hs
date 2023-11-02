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
        roots
        ( Node
            muspelheim
            (mkTerminalNode giantFight)
            Leaf
            Leaf
            Leaf
        )
        (mkTerminalNode mimirs_lake)
        (mkTerminalNode helheim)
        ( Node
            midgard
            (mkTerminalNode asgard)
            (mkTerminalNode alvheim)
            Leaf
            Leaf
        )
    )

-- -- Show children
-- previewTree :: QuadTree TreeNode -> [String]
-- previewTree Leaf = []
-- previewTree (Node lab _ _ _ _) = [previewmsg lab]

-- displayChildren :: QuadTree TreeNode -> [String]
-- displayChildren Leaf = []
-- displayChildren (Node a ll l r rr) = previewTree ll ++ previewTree l ++ previewTree r ++ previewTree rr

-- | Continuosly prompts the player for input until a well formed action is provided
-- | Note: this function is context-blind, it does not check if the action is meaningful at
-- | the current player location, just that it is in the set of all actions.
getAction :: IO Action
getAction = do
  putStr ">> "
  input <- getLine
  case parse input of
    Nothing -> do putStrLn "This is not a valid action. Perhaps you misstyped?"; getAction
    Just a -> return a

-- | Print the nodes the user has visited so far
displayMap :: GameInstance -> IO ()
displayMap _ = putStrLn "TBD"

-- | Runs the main game loop
gameLoop :: GameInstance -> IO GameInstance
gameLoop current_game = do
  putStrLn (description current_node)
  case nodetype current_node of
    FightNode fightT defeatT life lifeName (Obj reward) ->
      if life > 0
        then do
          putStrLn fightT
          putStrLn ("It carries around a " ++ reward)
          putStrLn ("It seems to have " ++ show life ++ " " ++ lifeName ++ " left.")
        else putStrLn defeatT
    PlatformNode -> putStrLn "What do you wish to do?"
  action <- getAction
  -- implement check to see whether action is valid in this context
  new_game <- act action current_game
  gameLoop new_game
  where
    current_node = root (tree (gamezip current_game))

-- we need smth like 'entry' which runs when we enter the node TODO: what do you mean?
runGame :: IO ()
runGame = do
  gameLoop (Game startWorld (Player 10 1 []))
  return ()
