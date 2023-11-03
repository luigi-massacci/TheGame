{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lib where

import Constants
import Data.List
import GHC.Utils.Panic.Plain (PlainGhcException)
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
      putStrLn "This is not a valid action. Perhaps you misstyped?"; getAction
    Just a -> return a

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
transition :: TreeZip Level -> String -> Maybe (TreeZip Level)
-- NOTE: I think this whole case should never run
transition (TreeZip ctx Leaf) destination =
  if destination == "back"
    then Just (goBack (TreeZip ctx Leaf))
    else Nothing
transition (TreeZip current_location (Node current_world ll l r rr)) destination
  | destination == "back" =
      Just (goBack (TreeZip current_location (Node current_world ll l r rr)))
  | matchingName ll destination =
      Just (TreeZip (LL current_world current_location l r rr) ll)
  | matchingName l destination =
      Just (TreeZip (L current_world ll current_location r rr) l)
  | matchingName r destination =
      Just (TreeZip (R current_world ll l current_location rr) r)
  | matchingName rr destination =
      Just (TreeZip (RR current_world ll l r current_location) rr)
  | otherwise = Nothing

-- | Move focus to the parent node of the currently focused node
goBack :: TreeZip Level -> TreeZip Level
goBack (TreeZip TOP tree) = TreeZip TOP tree
goBack (TreeZip (LL root ctx l r rr) ll) = TreeZip ctx (Node root ll l r rr)
goBack (TreeZip (L root ll ctx r rr) l) = TreeZip ctx (Node root ll l r rr)
goBack (TreeZip (R root ll l ctx rr) r) = TreeZip ctx (Node root ll l r rr)
goBack (TreeZip (RR root ll l r ctx) rr) = TreeZip ctx (Node root ll l r rr)

-- |  modifyPos <context> <updated_node> -> <updated_context>
-- |  Update the currently focused node
modifyPos :: TreeZip Level -> Level -> TreeZip Level
modifyPos (TreeZip ctx Leaf) new_world = TreeZip ctx (mkTerminalNode new_world)
modifyPos (TreeZip ctx (Node old_world ll l r rr)) new_world = TreeZip ctx (Node new_world ll l r rr)

-- | Tells if you are allowed to go to the node in the current GameInstance
unlocked :: Player -> TreeZip Level -> Bool
unlocked _ (TreeZip ctx Leaf) = False -- Can't go to leaves, should not happen anyway
unlocked (Player _ _ inventory) (TreeZip _ (Node level _ _ _ _)) =
  intersect (necessaryItems level) inventory == necessaryItems level

--------------------------------------------------------------------------------

-- | FIGHT FUNCTIONS

--------------------------------------------------------------------------------

-- | Determines the cycle of attackDamage winners
playerVictory :: AttackType -> AttackType -> Maybe Bool -- wins Rock Scissors -> True
playerVictory p1_move p2_move
  | p1_move == p2_move = Nothing
  | (p1_move == Rock && p2_move == Scissors)
      || (p1_move == Scissors && p2_move == Paper)
      || (p1_move == Paper && p2_move == Rock) =
      Just True
  | otherwise = Just False

-- | Modify enemy lifePoints
damage :: QuadTree Level -> Int -> QuadTree Level
damage (Node (Level name (Fight lifePoints ob) msg i) j k l m) d =
  Node (Level name (Fight (lifePoints - d) ob) msg i) j k l m
damage a i = a

-- | Checks enemy death and adds object to the player inventory
checkDeath :: Player -> Int -> Object -> IO Player
checkDeath p lifePoints "" =
  if attackDamage p < lifePoints
    then return p
    else do
      putStrLn "You defeated your enemy !"
      return p
checkDeath (Player l att inv) lifePoints s =
  if att < lifePoints
    then return (Player l att inv)
    else do
      putStrLn "You defeated your enemy !"
      putStrLn ("You received the " ++ s ++ ", well done !")
      addEffect (Player l att (s : inv)) s

-- | Adds object effect to the player
addEffect :: Player -> String -> IO Player
addEffect (Player lifePoints att inv) s = case s of
  "stone pickaxe" -> do putStrLn "It gives you more strength !"; return (Player lifePoints (att + 1) inv)
  _ -> return (Player lifePoints att inv)

lifePointsBuffs :: Player -> Int
lifePointsBuffs player = if "armor" `elem` inventory player then 5 else 0

attackBuffs :: Player -> Int
attackBuffs player = if "sword" `elem` inventory player then 2 else 0

--------------------------------------------------------------------------------

-- | MAIN ELABORATION FUNCTION

--------------------------------------------------------------------------------

evolve :: Action -> GameInstance -> IO GameInstance
evolve Help game = do
  displayMessage _HELP_MSG game
evolve Look game = do
  displayMessage "The look command is yet to be implemented." game
evolve (Move destination) (Game current_world player) =
  case transition current_world destination of
    Nothing ->
      displayMessage
        "It would seem the place you want to go to does not exist,\n\
        \or is inaccessible from your current location."
        (Game current_world player)
    Just new_world ->
      if unlocked player new_world
        then return (Game new_world player)
        else do
          putStrLn "You are not worthy enough to do this right now...\n"
          return (Game current_world player)
evolve (Attack player_move) (Game current_world player) =
  case tree current_world of
    Node (Level _ (Fight enemy_life_points item) _ _) _ _ _ _ ->
      if enemy_life_points <= 0
        then
          displayMessage
            "There is no one to fight here."
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
                ( "You won this round! Your did "
                    ++ show (attackDamage player + attackBuffs player)
                    ++ " points of damage."
                )
              np <- checkDeath player enemy_life_points item
              return (Game (TreeZip (context current_world) (damage (tree current_world) (attackDamage np))) np)
            Just False -> do
              putStrLn
                ( "You were no match for your opponent. You have "
                    ++ show (lifePoints player - 1)
                    ++ " life points left."
                )
              return (Game current_world (Player (lifePoints player - 1) (attackDamage player) (inventory player)))
            Nothing -> do putStrLn "Neither managed to best the other"; return (Game current_world player)
    _ ->
      displayMessage
        "This is no place for violence."
        (Game current_world player)
