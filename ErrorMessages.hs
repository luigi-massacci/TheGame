module ErrorMessages where

_HELP_MSG :: String =
  "\nWelcome! \n\
  \You can use the following commands: \n\
  \1) <help>: Displays this message\n\n\
  \Navigation:\n\
  \2) <go to PLACE_NAME>: Moves to PLACE_NAME. You will be told the possible places you can go to \n\
  \                       whenever you enter a node.\n\
  \3) <go back>: Return to the parent node\n\
  \4) <show map> : Prints the part of the tree you have currently explored\n\
  \5) <look> : TBD \n\n\
  \Fight: \n\
  \6) <rock> or <paper> or <scissors>: Plays the respective choice."

_ILLFORMED_INPUT :: String = "\nThis is not a valid action. Perhaps you misstyped?"

_INVALID_MOVE :: String =
  "\nIt would seem the place you want to go to does not exist, \
  \or is inaccessible from your current location."

_INVALID_ATTACK :: String = "\nThis is no place for violence."

_UNIMPLEMENTED :: String = "\nIt would appear this feature has yet to be implemented :-) "