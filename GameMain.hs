{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GameMain where

import Data.List
import Data.Set (fromList)
import GHC.Core.TyCon (newTyConEtadArity)
import Levels
import Lib
import StartPlayer
import StartWorld
import System.Random ()
import Types

-- | Print the nodes the user has visited so far
displayMap :: GameInstance -> IO ()
displayMap _ = putStrLn "TBD"

-- | Runs the main game input - eval loop
gameLoop :: GameInstance -> IO GameInstance
gameLoop current_game = do
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

-- | Game Intro point
runGame :: IO ()
runGame = do
  gameLoop (Game _START_WORLD _START_PLAYER)
  return ()
