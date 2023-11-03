{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Game where

import Constants
import Data.List
import Data.Set (fromList)
import GHC.Core.TyCon (newTyConEtadArity)
import Levels
import Parser
import System.Random ()
import Types

_START_WORLD :: TreeZip Level =
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

-- | Continuosly prompts the player for input until a well formed action is provided.
-- | Note: this function is context-blind, it does not check if the action is meaningful at
-- | the current player location, just that it is in the set of all actions.
getAction :: IO Action
getAction = do
  putStr ">> "
  input <- getLine
  case parse input of
    Nothing -> do
      putStrLn "This is not a valid action. Perhaps you misstyped?"; getAction
    Just a -> return a

-- | Print the nodes the user has visited so far
displayMap :: GameInstance -> IO ()
displayMap _ = putStrLn "TBD"

-- | Runs the main game loop
gameLoop :: GameInstance -> IO GameInstance
gameLoop current_game = do
  putStrLn (description current_node)
  case leveltype current_node of
    Fight fightT defeatT life lifeName reward ->
      if life > 0
        then do
          putStrLn fightT
          putStrLn ("It carries around a " ++ reward)
          putStrLn ("It seems to have " ++ show life ++ " " ++ lifeName ++ " left.")
        else putStrLn defeatT
    Platform -> putStrLn "What do you wish to do?"
  player_action <- getAction
  new_game <- evolve player_action current_game
  gameLoop new_game
  where
    current_node = root (tree (gamezip current_game))

-- we need smth like 'entry' which runs when we enter the node TODO: what do you mean?
runGame :: IO ()
runGame = do
  gameLoop (Game _START_WORLD (Player 5 1 []))
  return ()
