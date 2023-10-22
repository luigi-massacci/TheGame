{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Nodes where

import Data.List
import Data.Set (fromList)
import Data.Tree
import GHC.Core.TyCon (newTyConEtadArity)
import System.Random
import Types

-- Initial nodes

root :: TreeNode = TreeNode "Root" PlatformNode "You are at the root of Yggdrasil !" "You see some Root going upwards..."

midgard :: TreeNode = TreeNode "Midgard" PlatformNode "You've reached Midgard, the mortal plane!" "Maybe you can climb to Midgard."

asgard :: TreeNode = TreeNode "Asgard" PlatformNode "You've reached Asgard, home of the Gods!" "The bifrost lays beneath your eyes! This is your way to Asgard!"

swartelheim :: TreeNode = TreeNode "Swartelheim" PlatformNode "You've reached Swartelheim, land of the dwarves!" "You see some mountains in the distance. Could this be Swartelheim?"

muspelheim :: TreeNode = TreeNode "Muspelheim" PlatformNode "You've reached Muspelheim, land of the fire giants!" "You can feel hot wind coming from this direction. If it is Muspelheim, it might be dangerous..."

helheim :: TreeNode = TreeNode "Helheim" PlatformNode "You've reached Helheim, land of the dead!" "This should definitely never be printed"

alvheim :: TreeNode = TreeNode "Alfheim" PlatformNode "You've reached Alvehim, land of the elves" "This forest looks deep and dangerous. This is not what you would expect Alfheim to look like"

{-
yggdrasil :: Tree TreeNode =
  Node
    root
    [ Node midgard [Node asgard [], Node alvheim []],
      Node swartelheim [],
      Node muspelheim [],
      Node helheim []
    ]
-}
