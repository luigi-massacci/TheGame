{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Nodes where

import Constants
import Data.List
import Data.Set (fromList)
import Data.Tree
import GHC.Core.TyCon (newTyConEtadArity)
import System.Random
import Types

-- Initial nodes

roots :: TreeNode = TreeNode "ROOTS" PlatformNode _ROOTS_MSG

midgard :: TreeNode = TreeNode "MIDGARD" PlatformNode "You've reached Midgard, the mortal plane!" "Maybe you can climb to Midgard."

asgard :: TreeNode = TreeNode "ASGARD" PlatformNode "You've reached Asgard, home of the Gods!" "The bifrost lays beneath your eyes! This is your way to Asgard!"

swartelheim :: TreeNode = TreeNode "SWARTELHEIM" PlatformNode "You've reached Swartelheim, land of the dwarves!" "You see some mountains in the distance. Could this be Swartelheim?"

muspelheim :: TreeNode = TreeNode "MUSPELHEIM" PlatformNode "You've reached Muspelheim, land of the fire giants!" "You can feel hot wind coming from this direction. If it is Muspelheim, it might be dangerous..."

helheim :: TreeNode = TreeNode "HELHEIM" PlatformNode "You've reached Helheim, land of the dead!" "This should definitely never be printed"

alvheim :: TreeNode = TreeNode "ALFHEIM" PlatformNode "You've reached Alvehim, land of the elves" "This forest looks deep and dangerous. This is not what you would expect Alfheim to look like"

giantFight :: TreeNode = TreeNode "Giant House" (FightNode "Oh god, there's a fire giant in front of you. It is big." "The fire giant's dead body in front of you. It actually looks like the mountains surrounding. This creeps you out." 5 "hearts" (Obj "stone pickaxe")) "There's a giant stone house in front of you" "You can distinguish some kind of Giant House in the distance"
