{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Parser where

import Types

parse :: String -> Maybe Action
parse "" = Nothing
parse s
  | take 7 s == "go back" = Just (Move "back")
  | take 6 s == "go to " = Just (Move (drop 6 s))
  | otherwise = case s of
      "rock"    -> Just (Attack Rock)
      "paper"   -> Just (Attack Paper)
      "scissors"-> Just (Attack Scissors)
      "look" -> Just Look
      _ -> Nothing



matching_names :: QuadTree TreeNode -> String -> Bool
matching_names Leaf s = False
matching_names n s = name (label n) == s


-- Take path to node of String or backwards if String is "back"
maybe_take_path :: Maybe (TreeZip TreeNode) -> String -> Maybe (TreeZip TreeNode)
maybe_take_path Nothing s = Nothing
maybe_take_path (Just c) s = take_path c s

take_path :: TreeZip TreeNode -> String -> Maybe (TreeZip TreeNode)
take_path (TreeZip ctx Leaf) s = if s == "back" then Just (go_back (TreeZip ctx Leaf)) else Nothing  -- Should never be run; Leaves are empty
take_path (TreeZip ctx (Node label ll l r rr)) s
  | s == "back" = Just (go_back (TreeZip ctx (Node label ll l r rr)))
  | matching_names ll s = Just (TreeZip (LL label ctx l r rr) ll)
  | matching_names l  s = Just (TreeZip (L  label ll ctx r rr) l)
  | matching_names r  s = Just (TreeZip (R  label ll l ctx rr) r)
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


act :: Action -> GameInstance -> Maybe GameInstance -- "Nothing" would mean nothing happened; but we could also return the same gameinstance
act Look game = Just game
act (Move s) (Game t p) = do {nt <- take_path t s; Just (Game nt p)} 
act (Attack t) game = undefined -- TODO this