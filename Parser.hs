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

-- When updating the tree, we just update parents backwards
act :: GameInstance -> Maybe Action -> Maybe GameInstance
act = undefined