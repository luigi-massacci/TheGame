{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lib where

import Data.List
import ErrorMessages
import GHC.Utils.Panic.Plain (PlainGhcException)
import StartPlayer
import StartWorld (_START_WORLD)
import System.Random
import Types

--------------------------------------------------------------------------------

-- | UI FUNCTIONS

--------------------------------------------------------------------------------

-- | Map player input into valid actions
-- | Note: when moving to a child note, the validity of the child name
-- |       is not checked at this stage (see <evolve> for that)
-- | If the input is illformed, returns Nothing.
-- | See <getAction> for the handling of this error.
parse :: String -> Maybe Action
parse "" = Nothing
parse input
  | take 7 input == "go back" = Just (Move "back")
  | take 6 input == "go to " = Just (Move (drop 6 input))
  | take 4 input == "look" = Just Look
  | take 4 input == "help" = Just Help
  | take 8 input == "show map" = Just ShowMap
  | otherwise = case input of
      "rock" -> Just (Attack Rock)
      "paper" -> Just (Attack Paper)
      "scissors" -> Just (Attack Scissors)
      _ -> Nothing

-- | Continuosly prompts the player for input until a well formed action is provided.
-- | Note: this function is context-blind, it does not check if the action is meaningful at
-- | the current player location, just that it is in the set of all actions.
-- | (see <evolve> for the error handling)
getAction :: IO Action
getAction = do
  putStr ">> "
  input <- getLine
  case parse input of
    Nothing -> do
      putStrLn _ILLFORMED_INPUT; getAction
    Just a -> return a

-- | Displays message and returns the game instance unchanged
displayMessage :: String -> GameInstance -> IO GameInstance
displayMessage message game = do
  putStrLn message
  putStr "Enter anything to continue: "
  input <- getLine
  return game

--------------------------------------------------------------------------------

-- | TREE NAVIGATION FUNCTIONS

--------------------------------------------------------------------------------

-- | Cheks that the name of the input level matches the input string
-- | Needed to validate <go to location> instructions
matchingName :: QuadTree Level -> String -> Bool
matchingName Leaf target_location = False
matchingName level target_location = name (root level) == target_location

-- | Compute result of <go *> instruction
-- | Returns Nothing if the destination does not match any accessssible child
-- | or parent node.
transition :: TreeZip Level -> String -> IO (Maybe (TreeZip Level))
-- NOTE: The first case should be impossible to run into
transition (TreeZip ctx Leaf) destination = do
  if destination == "back"
    then return (Just (goBack (TreeZip ctx Leaf)))
    else return Nothing
transition (TreeZip current_location (Node current_world ll l r rr)) destination
  | (destination == "UPPER BRANCH")||(destination == "LOWER BRANCH") = do
    if matchingName ll destination
      then do
        new_tree <- mkTripleTree (root ll)
        return (Just (TreeZip (LL current_world current_location l r rr) new_tree))
      else if matchingName l destination
        then do
          new_tree <- mkTripleTree (root l)
          return (Just (TreeZip (L current_world ll current_location r rr) new_tree))
        else if matchingName r destination
          then do
            new_tree <- mkTripleTree (root r)
            return (Just (TreeZip (R current_world ll l current_location rr) new_tree))
          else if matchingName rr destination
            then do
              new_tree <- mkTripleTree (root rr)
              return (Just (TreeZip (RR current_world ll l r current_location) new_tree))
            else return Nothing
  | destination == "back" = return (Just (goBack (TreeZip current_location (Node current_world ll l r rr))))
  | matchingName ll destination = return (Just (TreeZip (LL current_world current_location l r rr) ll))
  | matchingName l destination = return (Just (TreeZip (L current_world ll current_location r rr) l))
  | matchingName r destination = return (Just (TreeZip (R current_world ll l current_location rr) r))
  | matchingName rr destination = return (Just (TreeZip (RR current_world ll l r current_location) rr))
  | otherwise = return Nothing

-- | Move focus to the parent node of the currently focused node
goBack :: TreeZip Level -> TreeZip Level
goBack (TreeZip TOP tree) = TreeZip TOP tree
goBack (TreeZip (LL root ctx l r rr) ll) = TreeZip ctx (Node root ll l r rr)
goBack (TreeZip (L root ll ctx r rr) l) = TreeZip ctx (Node root ll l r rr)
goBack (TreeZip (R root ll l ctx rr) r) = TreeZip ctx (Node root ll l r rr)
goBack (TreeZip (RR root ll l r ctx) rr) = TreeZip ctx (Node root ll l r rr)

-- |  modifyPos <context> <updated_node> -> <updated_context>
-- |  Update the currently focused node
modifyPos :: TreeZip Level -> Level -> IO (TreeZip Level)
modifyPos (TreeZip ctx Leaf) new_world = do 
                                          terminal_node <- mkTerminalNode new_world
                                          return (TreeZip ctx (terminal_node))
modifyPos (TreeZip ctx (Node old_world ll l r rr)) new_world = do return (TreeZip ctx (Node new_world ll l r rr))

-- | Checks if the conditions to enter the node are met
-- | Implementation note: at the moment this checks
-- |                      whether some item is in the inventory
unlocked :: Player -> TreeZip Level -> Bool
unlocked _ (TreeZip ctx Leaf) = False -- Can't go to leaves, should not happen anyway
unlocked (Player _ _ inventory) (TreeZip _ (Node level _ _ _ _)) =
  intersect (necessaryItems level) inventory == necessaryItems level

-- | Moves the focus to Helheim, leaving the tree unchanged
respawnPlayer :: TreeZip Level -> TreeZip Level
respawnPlayer t = t

--------------------------------------------------------------------------------

-- | FIGHT FUNCTIONS

--------------------------------------------------------------------------------

-- | Determines who won the round of rock-paper-scissors
-- | player wins -> True
-- | enemy wins -> False
-- | draw -> Nothing
playerVictory :: AttackType -> AttackType -> Maybe Bool
playerVictory player_move enemy_move
  | player_move == enemy_move = Nothing
  | (player_move == Rock && enemy_move == Scissors)
      || (player_move == Scissors && enemy_move == Paper)
      || (player_move == Paper && enemy_move == Rock) =
      Just True
  | otherwise = Just False

-- | Adds item to the player inventory, and updates
-- | the player stats based on the specific item
updatePlayer :: Player -> Object -> IO Player
updatePlayer (Player life_points attack_damage inventory) item =
  case item of
    "sword" -> do
      putStrLn "Your blows now deal 2 extra damage."
      return (Player life_points (attack_damage + 2) new_inventory)
    "armor" -> do
      putStrLn "You now have 5 extra life points."
      return (Player (life_points + 5) attack_damage new_inventory)
    _ -> do
      putStrLn (item ++ " has been added to your inventory.")
      return (Player life_points attack_damage new_inventory)
  where
    new_inventory = item : inventory

-- | Update a fight level after the player has won a round.
-- | If the enemy is defeated, replaces the level description with the new text
-- | to be used after victory, and triggers the reward. Otherwise, updates
-- | the enemy's health. Note: enemies do not regenerate health.

updateWorld :: QuadTree Level -> Player -> IO (QuadTree Level, Player)
updateWorld (Node (Level n (Fight victory_text enemy_life_points item) description lm i _) ll l r rr) player =
  if remaining_enemy_life <= 0
    then do
      putStrLn "You have vanquished your enemy."
      putStrLn ("As he flees, he leaves his " ++ item ++ " behind.")
      putStrLn "You take it for yourself."
      new_player <- updatePlayer player item
      return (victory_world, new_player)
    else return (new_world, player)
  where
    remaining_enemy_life = enemy_life_points - attackDamage player
    victory_world = Node (Level n (Fight victory_text remaining_enemy_life item) victory_text lm i True) l ll r rr
    new_world = Node (Level n (Fight victory_text remaining_enemy_life item) description lm i True) l ll r rr
updateWorld level player = return (level, player)

--------------------------------------------------------------------------------

-- | MAIN ELABORATION FUNCTION

--------------------------------------------------------------------------------

-- | Updates the game based on the player action.
-- | Handles eventual invalid actions.
evolve :: Action -> GameInstance -> IO GameInstance
evolve Help game = do
  displayMessage _HELP_MSG game
evolve Look game = do
  displayMessage _UNIMPLEMENTED game
evolve ShowMap game = do
  displayMap game
  
evolve (Move destination) (Game current_world player) = do
  tr <- transition current_world destination
  case tr of
    Nothing ->
      displayMessage _INVALID_MOVE (Game current_world player)
    Just new_world ->
      if unlocked player new_world
        then return (Game new_world player)
        else do
          putStrLn (lockMessage (root (tree new_world)))
          return (Game current_world player)
evolve (Attack player_move) (Game current_world player) =
  case leveltype current_node of
    Fight victory_text enemy_life_points item ->
      if enemy_life_points <= 0
        then
          displayMessage
            "\nThere is no one to fight here anymore.\n"
            (Game current_world player)
        else do
          move_int <- randomRIO (1, 3) :: IO Int
          let enemy_move =
                ( case move_int of
                    1 -> Rock
                    2 -> Paper
                    3 -> Scissors
                )
          putStrLn ("Your enemy has played " ++ show enemy_move)
          case playerVictory player_move enemy_move of
            Just True -> do
              putStrLn
                ( "You won this round! You did "
                    ++ show (attackDamage player)
                    ++ " points of damage."
                )
              (new_level, new_player) <- updateWorld (tree current_world) player
              return (Game (TreeZip (context current_world) new_level) new_player)
            Just False -> do
              putStrLn
                ( "\nYou were no match for your opponent. You have "
                    ++ show (lifePoints player - 1)
                    ++ " life points left."
                )
              if (lifePoints player - 1) == 0
                then do
                  putStrLn "\nOh No! You died! Perhaps you will have better luck in your next life."
                  start_world <- _START_WORLD
                  return (Game start_world _START_PLAYER)
                else
                  return
                    ( Game
                        current_world
                        ( Player
                            (lifePoints player - 1)
                            (attackDamage player)
                            (inventory player)
                        )
                    )
            Nothing -> do putStrLn "\nNeither managed to best the other"; return (Game current_world player)
    _ ->
      displayMessage _INVALID_ATTACK (Game current_world player)
  where
    current_node = root (tree current_world)

-- | Running through this changes color of the text, used for displaying map on show map
setColor :: String -> String -> String
setColor color_code text = "\x1b[" ++ color_code ++ "m" ++ text ++ "\x1b[0m"

-- | Plugs into the hole (for reconstructing the tree)
plug :: Context Level -> QuadTree Level -> QuadTree Level
plug TOP t = t
plug (LL n cntx t2 t3 t4) t = plug cntx (Node n t t2 t3 t4)
plug (L n t1 cntx t3 t4) t = plug cntx (Node n t1 t t3 t4)
plug (R n t1 t2 cntx t4) t = plug cntx (Node n t1 t2 t t4)
plug (RR n t1 t2 t3 cntx) t = plug cntx (Node n t1 t2 t3 t)

-- | Reconstructs the full (so far generated) map
reconstruct :: TreeZip Level -> QuadTree Level
reconstruct gamezip = plug (context gamezip) (new_tree) where
                          new_tree = case (tree gamezip) of 
                            Leaf -> Leaf
                            Node n t1 t2 t3 t4 -> Node (n { name = (name n) ++ (setColor "34" " <- You are here")}) t1 t2 t3 t4

-- | Given a quadtree draws a tree. Visited nodes are in green, Non-visited nodes are in red.
drawQuadTree :: QuadTree Level -> String
drawQuadTree tree = draw tree 0
  where
    draw :: QuadTree Level -> Int -> String
    draw Leaf _ = ""
    draw (Node root ll l r rr) depth =
      replicate (depth*5) ' ' ++ replicate (depth) '\\' ++ (setColor color (name root)) ++ "\n" ++
      drawChild "LL" ll ++
      drawChild "L " l ++
      drawChild "R " r ++
      drawChild "RR" rr
      where
        drawChild name child = draw child (depth + 1)
        color = if (visited (root)) then "32" else "31"

-- | Final function for displaying the map
displayMap :: GameInstance -> IO GameInstance
displayMap game = do
    putStrLn (drawQuadTree (reconstruct (gamezip game)))
    putStr "Enter anything to continue: "
    input <- getLine
    return game