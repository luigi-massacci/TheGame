module Parser where

import Types

parse :: String -> Maybe Action
parse "" = Nothing
parse s =
  if take 6 s == "go to "
    then Just (Move (drop 6 s))
    else case s of
      "rock"    -> Just (Attack Rock)
      "paper"   -> Just (Attack Paper)
      "scissors"-> Just (Attack Scissors)
      "look" -> Just Look
      _ -> Nothing



matching_names :: QuadTree TreeNode -> String -> Bool
matching_names Leaf s = False
matching_names n s = name (label n) == s


-- Take path to node of String or backwards if String is "back"
take_path :: TreeZip TreeNode -> String -> Maybe (TreeZip TreeNode)
take_path (TreeZip ctx Leaf) s = if s == "back" then Just (go_back (TreeZip ctx Leaf)) else Nothing  -- Should never be run; Leaves are empty
take_path (TreeZip ctx Node label ll l r rr) s = if s == "back" then Just (go_back (TreeZip ctx Node label ll l r rr)) else
  if matching_names ll s then Just ((LL label ctx l r rr), ll) else
  if matching_names l  s then Just ((L  label ll ctx r rr), l) else
  if matching_names r  s then Just ((R  label ll l ctx rr), r) else
  if matching_names rr s then Just ((RR label ll l r ctx), rr) else Nothing


-- Goes back to the node parent
go_back :: TreeZip TreeNode -> TreeZip TreeNode
go_back (TreeZip TOP t) = t
go_back (TreeZip (LL label ctx l r rr) ll) = TreeZip ctx (Node label ll l r rr)
go_back (TreeZip (L label ll ctx r rr) ll) = TreeZip ctx (Node label ll l r rr)
go_back (TreeZip (R label ll l ctx rr) ll) = TreeZip ctx (Node label ll l r rr)
go_back (TreeZip (RR label ll l r ctx) ll) = TreeZip ctx (Node label ll l r rr)


-- Modify the tree by putting the second argument in the context of the first argument
modify_pos :: TreeZip TreeNode -> TreeNode -> TreeZip Treenode
modify_pos (TreeZip ctx Leaf) t = (TreeZip ctx (Node t Leaf Leaf Leaf Leaf))
modify_pos (TreeZip ctx (Node lab ll l r rr)) t = (TreeZip ctx (Node t ll l r rr))


act :: GameInstance -> Maybe Action -> Maybe GameInstance
act = undefined
