{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.List
import Data.Set (fromList)
import GHC.Core.TyCon (newTyConEtadArity)
import Levels
import Lib
import StartPlayer
import StartWorld
import System.Random ()
import Types


markVisited :: GameInstance -> GameInstance
markVisited game = Game
  { gamezip = updatedTreeZip,
    player = player game
  }
  where
    new_root = (root (tree (gamezip game))) {visited = True}
    new_tree = (tree (gamezip game)) {root = new_root}
    updatedTreeZip = (gamezip game) { tree = new_tree }

-- | Runs the main game input - eval loop
gameLoop :: GameInstance -> IO GameInstance
gameLoop game = do
  putStrLn (description current_node)
  case leveltype current_node of
    Fight vt enemy_life_points _ ->
      if enemy_life_points > 0
        then do
          putStrLn "\nYou have been challenged to a game of rock-paper-scissors."
          putStrLn
            ( "Your enemy has "
                ++ show enemy_life_points
                ++ " life points."
            )
          putStrLn
            ( "You have "
                ++ show (lifePoints (player current_game))
                ++ " life points."
            )
          putStrLn "Choose your move wisely."
        else putStrLn ""
    Platform -> putStrLn "What do you wish to do?"
  player_action <- getAction
  new_game <- evolve player_action current_game
  gameLoop new_game
  where
    current_node = root (tree (gamezip current_game))
    current_game = markVisited game

-- | Game Intro point
runGame :: IO ()
runGame = do
  gameLoop (Game _START_WORLD _START_PLAYER)
  return ()

main = runGame