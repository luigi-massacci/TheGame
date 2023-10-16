module Parser where

import Types

parse :: String -> Maybe Action
parse "" = Nothing
parse s =
  if take 6 s == "go to "
    then Just (Act Move (drop 6 s))
    else case s of
      s | s == "rock" || s == "paper" || s == "scissors" -> Just (Act Attack s)
      s | s == "look" -> Just (Act Look s)
      _ -> Nothing

-- When updating the tree, we just update parents backwards
act :: GameInstance -> Maybe Action -> Maybe GameInstance
act = undefined