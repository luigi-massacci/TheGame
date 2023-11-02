{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Parser where

import Types

-- | Map player input into valid actions
-- | Note: when moving to a child note, the validity of the child name
-- |       is not checked at this stage
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


-- | Checks that the location of a "go to <child_name>" instruction is valid
matchingName :: QuadTree TreeNode -> String -> Bool
matchingName Leaf s = False
matchingName n s = name (label n) == s

-- Take path to node of String or backwards if String is "back"
take_path :: TreeZip TreeNode -> String -> Maybe (TreeZip TreeNode)
take_path (TreeZip ctx Leaf) s = if s == "back" then Just (goBack (TreeZip ctx Leaf)) else Nothing -- Should never be run; Leaves are empty
take_path (TreeZip ctx (Node label ll l r rr)) s
  | s == "back" = Just (goBack (TreeZip ctx (Node label ll l r rr)))
  | matchingName ll s = Just (TreeZip (LL label ctx l r rr) ll)
  | matchingName l s = Just (TreeZip (L label ll ctx r rr) l)
  | matchingName r s = Just (TreeZip (R label ll l ctx rr) r)
  | matchingName rr s = Just (TreeZip (RR label ll l r ctx) rr)
  | otherwise = Nothing

-- | Shift context to parent node
goBack :: TreeZip TreeNode -> TreeZip TreeNode
goBack (TreeZip TOP t) = TreeZip TOP t
goBack (TreeZip (LL label ctx l r rr) ll) = TreeZip ctx (Node label ll l r rr)
goBack (TreeZip (L label ll ctx r rr) l) = TreeZip ctx (Node label ll l r rr)
goBack (TreeZip (R label ll l ctx rr) r) = TreeZip ctx (Node label ll l r rr)
goBack (TreeZip (RR label ll l r ctx) rr) = TreeZip ctx (Node label ll l r rr)

-- |  modifyPos <context> <updated_node> -> <updated_context>
--    Update currently focused node 

modifyPos :: TreeZip TreeNode -> TreeNode -> TreeZip TreeNode
modifyPos (TreeZip ctx Leaf) t = (TreeZip ctx (Node t Leaf Leaf Leaf Leaf))
modifyPos (TreeZip ctx (Node lab ll l r rr)) t = (TreeZip ctx (Node t ll l r rr))

-- | Checks whether the player meets the requirements to access a given node
isAccessible :: GameInstance -> TreeZip TreeNode -> Bool
isAccessible _ _ = True

act :: Action -> GameInstance -> IO GameInstance
act Help game = do 
    putStrLn _HELP_MSG
    return game
act Look game = return game
act (Move s) (Game t p) = case take_path t s of
  Nothing -> return (Game t p)
  Just nt ->
    if isAccessible (Game t p) nt
      then return (Game nt p)
      else do
        putStrLn "You are not worthy enough to do this right now...\n" -- replace with message specific to node
        return (Game t p)
act (Attack t) game = undefined -- TODO
