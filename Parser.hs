{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Parser where

import Types
import Data.List

import System.Random
import GHC.Utils.Panic.Plain (PlainGhcException)

-- Good
parse :: String -> Maybe Action
parse "" = Nothing
parse s
  | take 7 s == "go back" = Just (Move "back")
  | take 6 s == "go to " = Just (Move (drop 6 s))
  | take 4 s == "look" = Just Look
  | take 4 s == "help" = Just Help
  | take 8 s == "show map" = Just ShowMap
  | otherwise = case s of
      "rock" -> Just (Attack Rock)
      "paper" -> Just (Attack Paper)
      "scissors" -> Just (Attack Scissors)
      _ -> Nothing

matching_names :: QuadTree TreeNode -> String -> Bool
matching_names Leaf s = False
matching_names n s = name (label n) == s

-- Take path to node of String or backwards if String is "back"
take_path :: TreeZip TreeNode -> String -> Maybe (TreeZip TreeNode)
take_path (TreeZip ctx Leaf) s = if s == "back" then Just (go_back (TreeZip ctx Leaf)) else Nothing -- Should never be run; Leaves are empty
take_path (TreeZip ctx (Node label ll l r rr)) s
  | s == "back" = Just (go_back (TreeZip ctx (Node label ll l r rr)))
  | matching_names ll s = Just (TreeZip (LL label ctx l r rr) ll)
  | matching_names l s = Just (TreeZip (L label ll ctx r rr) l)
  | matching_names r s = Just (TreeZip (R label ll l ctx rr) r)
  | matching_names rr s = Just (TreeZip (RR label ll l r ctx) rr)
  | otherwise = Nothing

-- Goes back to the node parent
go_back :: TreeZip TreeNode -> TreeZip TreeNode
go_back (TreeZip TOP t) = TreeZip TOP t
go_back (TreeZip (LL label ctx l r rr) ll) = TreeZip ctx (Node label ll l r rr)
go_back (TreeZip (L label ll ctx r rr) l) = TreeZip ctx (Node label ll l r rr)
go_back (TreeZip (R label ll l ctx rr) r) = TreeZip ctx (Node label ll l r rr)
go_back (TreeZip (RR label ll l r ctx) rr) = TreeZip ctx (Node label ll l r rr)

-- Modify the tree by putting the second argument in the context of the first argument
modify_pos :: TreeZip TreeNode -> TreeNode -> TreeZip TreeNode
modify_pos (TreeZip ctx Leaf) t = (TreeZip ctx (Node t Leaf Leaf Leaf Leaf))
modify_pos (TreeZip ctx (Node lab ll l r rr)) t = (TreeZip ctx (Node t ll l r rr))

-- Tells if you are allowed to go to the node in the current GameInstance
authorizedMove :: Player -> TreeZip TreeNode -> Bool
authorizedMove _ (TreeZip ctx Leaf) = False -- Can't go to leaves, should not happen anyway
authorizedMove (Player l att inv) (TreeZip ctx (Node n a b c d)) = intersect (necessary_items n) (inv) == necessary_items n


-- Determines the cycle of attack winners
wins :: AttackType -> AttackType -> Maybe Bool -- wins Rock Scissors -> True
wins a b
  | (a==b) = Nothing
  | (a==Rock && b==Scissors) || (a==Scissors && b==Paper) || (a==Paper && b==Rock) = Just True
  | otherwise = Just False

-- Modify ennemy life
damage :: QuadTree TreeNode -> Int -> QuadTree TreeNode
damage (Node (TreeNode n (FightNode ft dt life ln ob) msg p i) j k l m) d = (Node (TreeNode n (FightNode ft dt (life - d) ln ob) msg p i) j k l m)
damage a i = a

-- Checks ennemy death and adds object to the player inventory
checkDeath :: Player -> Int -> Object -> IO Player 
checkDeath p life (Obj "") = if attack p <life then return p else do {
                                                putStrLn "You defeated your ennemy !";
                                                return p}
checkDeath (Player l att inv) life (Obj s) =  if att<life then return (Player l att inv) else do {
                                                putStrLn "You defeated your ennemy !";
                                                putStrLn ("You received the " ++ s ++ ", well done !");
                                                addEffect (Player l att (Obj s:inv)) s}

-- Adds object effect to the player
addEffect :: Player -> String -> IO Player
addEffect (Player life att inv) s = case s of
  "stone pickaxe" -> do {putStrLn "It gives you more strength !"; return (Player life (att+1) inv)}
  _ -> return (Player life att inv)

act :: Action -> GameInstance -> IO GameInstance
act Help game = return game
act Look game = return game
act (Move s) (Game t p) = case take_path t s of
                            Nothing -> return (Game t p)
                            Just nt -> if authorizedMove p nt then return (Game nt p) else do {putStrLn "You are not worthy enough to do this right now...\n";
                                                                                                        return (Game t p)}
act (Attack t) (Game zip p) =
  case tree zip of
    Node (TreeNode _ (FightNode _ _ lifepoints _ obj) _ _ _) _ _ _ _ -> if lifepoints <= 0 then return (Game zip p) else do
      move_int <- randomRIO(1, 3) :: IO Int
      let ennemy_move = ( case move_int of
                                    1 -> Rock
                                    2 -> Paper
                                    3 -> Scissors)
      putStrLn ("Your ennemy plays " ++ show ennemy_move)
      case wins t ennemy_move of
        Just True -> do { putStrLn ("You win ! You hurt him of " ++ show (attack p));
                          np <- checkDeath p lifepoints obj;
                          return (Game (TreeZip (context zip) (damage (tree zip) (attack np))) np)}
        Nothing -> do { putStrLn "It's a draw..."; return (Game zip p)}
        Just False -> do {putStrLn ("It hurts you ! You have " ++ show ((life p) - 1) ++ " prayers to Odin left.");
                          return (Game zip (Player ((life p)-1) (attack p) (inventory p)))}
    _ -> return (Game zip p)


