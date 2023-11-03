{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser where

import Constants
import Data.List
import GHC.Utils.Panic.Plain (PlainGhcException)
import System.Random
import Types

-- | Map player input into valid actions
-- | Note: when moving to a child note, the validity of the child name
-- |       is not checked at this stage (see <evolve> for that)
-- | If the input is illformed, returns Nothing
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

-- | Cheks that the name of the input level matches the input string
-- | Needed to validate <go to location> instructions
matchingName :: QuadTree Level -> String -> Bool
matchingName Leaf target_location = False
matchingName level target_location = name (root level) == target_location

-- | Take path to node of String or backwards if String is "back"
takePath :: TreeZip Level -> String -> Maybe (TreeZip Level)
takePath (TreeZip ctx Leaf) target =
  if target == "back"
    then Just (goBack (TreeZip ctx Leaf))
    else Nothing -- Should never be run; Leaves are empty
takePath (TreeZip ctx (Node root ll l r rr)) target
  | target == "back" = Just (goBack (TreeZip ctx (Node root ll l r rr)))
  | matchingName ll target = Just (TreeZip (LL root ctx l r rr) ll)
  | matchingName l target = Just (TreeZip (L root ll ctx r rr) l)
  | matchingName r target = Just (TreeZip (R root ll l ctx rr) r)
  | matchingName rr target = Just (TreeZip (RR root ll l r ctx) rr)
  | otherwise = Nothing

-- | Move focus to the parent node of the currently focused node
goBack :: TreeZip Level -> TreeZip Level
goBack (TreeZip TOP t) = TreeZip TOP t
goBack (TreeZip (LL root ctx l r rr) ll) = TreeZip ctx (Node root ll l r rr)
goBack (TreeZip (L root ll ctx r rr) l) = TreeZip ctx (Node root ll l r rr)
goBack (TreeZip (R root ll l ctx rr) r) = TreeZip ctx (Node root ll l r rr)
goBack (TreeZip (RR root ll l r ctx) rr) = TreeZip ctx (Node root ll l r rr)

-- |  modifyPos <context> <updated_node> -> <updated_context>
-- |  Update the currently focused node
modifyPos :: TreeZip Level -> Level -> TreeZip Level
modifyPos (TreeZip ctx Leaf) t = TreeZip ctx (Node t Leaf Leaf Leaf Leaf)
modifyPos (TreeZip ctx (Node lab ll l r rr)) t = TreeZip ctx (Node t ll l r rr)

-- | Tells if you are allowed to go to the node in the current GameInstance
authorizedMove :: Player -> TreeZip Level -> Bool
authorizedMove _ (TreeZip ctx Leaf) = False -- Can't go to leaves, should not happen anyway
authorizedMove (Player l att inv) (TreeZip ctx (Node n a b c d)) = intersect (necessary_items n) inv == necessary_items n

-- | Determines the cycle of attack_damage winners
wins :: AttackType -> AttackType -> Maybe Bool -- wins Rock Scissors -> True
wins p1_move p2_move
  | p1_move == p2_move = Nothing
  | (p1_move == Rock && p2_move == Scissors)
      || (p1_move == Scissors && p2_move == Paper)
      || (p1_move == Paper && p2_move == Rock) =
      Just True
  | otherwise = Just False

-- | Modify enemy life
damage :: QuadTree Level -> Int -> QuadTree Level
damage (Node (Level name (Fight ft dt life ln ob) msg i) j k l m) d =
  Node (Level name (Fight ft dt (life - d) ln ob) msg i) j k l m
damage a i = a

-- | Checks enemy death and adds object to the player inventory
checkDeath :: Player -> Int -> Object -> IO Player
checkDeath p life "" =
  if attack_damage p < life
    then return p
    else do
      putStrLn "You defeated your enemy !"
      return p
checkDeath (Player l att inv) life s =
  if att < life
    then return (Player l att inv)
    else do
      putStrLn "You defeated your enemy !"
      putStrLn ("You received the " ++ s ++ ", well done !")
      addEffect (Player l att (s : inv)) s

-- | Adds object effect to the player
addEffect :: Player -> String -> IO Player
addEffect (Player life att inv) s = case s of
  "stone pickaxe" -> do putStrLn "It gives you more strength !"; return (Player life (att + 1) inv)
  _ -> return (Player life att inv)

lifeBuffs :: Player -> Int
lifeBuffs player = if "armor" `elem` inventory player then 5 else 0

attackBuffs :: Player -> Int
attackBuffs player = if "sword" `elem` inventory player then 2 else 0

evolve :: Action -> GameInstance -> IO GameInstance
evolve Help game = do
  putStrLn _HELP_MSG
  putStr "Enter anything to go back to the game: "
  input <- getLine
  return game
evolve Look game = do
  putStrLn "The look command is yet to be implemented."
  return game
evolve (Move s) (Game t p) = case takePath t s of
  Nothing -> do
    putStrLn "It would seem the place you want to go to does not exist."
    putStr "Enter anything to continue: "
    input <- getLine
    return (Game t p)
  Just nt ->
    if authorizedMove p nt
      then return (Game nt p)
      else do
        putStrLn "You are not worthy enough to do this right now...\n"
        return (Game t p)
evolve (Attack t) (Game zip p) =
  case tree zip of
    Node (Level _ (Fight _ _ lifepoints _ obj) _ _) _ _ _ _ ->
      if lifepoints <= 0
        then return (Game zip p)
        else do
          move_int <- randomRIO (1, 3) :: IO Int
          let enemy_move =
                ( case move_int of
                    1 -> Rock
                    2 -> Paper
                    3 -> Scissors
                )
          putStrLn ("Your enemy plays " ++ show enemy_move)
          case wins t enemy_move of
            Just True -> do
              putStrLn ("You win ! You hurt him of " ++ show (attack_damage p))
              np <- checkDeath p lifepoints obj
              return (Game (TreeZip (context zip) (damage (tree zip) (attack_damage np))) np)
            Nothing -> do putStrLn "It's a draw..."; return (Game zip p)
            Just False -> do
              putStrLn ("It hurts you ! You have " ++ show (life p - 1) ++ " prayers to Odin left.")
              return (Game zip (Player (life p - 1) (attack_damage p) (inventory p)))
    _ -> do
      putStrLn "This is no place for violence."
      putStr "Enter anything to continue: "
      input <- getLine
      return (Game zip p)
