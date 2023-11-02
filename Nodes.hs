{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Nodes where

import Constants
import Data.List
import Data.Set (fromList)
import GHC.Core.TyCon (newTyConEtadArity)
import System.Random
import Types

-- Initial nodes

helheim :: TreeNode =
  TreeNode "HELHEIM" PlatformNode "You've reached Helheim, land of the dead!" "This should definitely never be printed" []

roots :: TreeNode =
  TreeNode "ROOTS" PlatformNode _ROOTS_DESCRIPTION "When is this printed? From roots preview " []

midgard :: TreeNode =
  TreeNode "MIDGARD" PlatformNode _MIDGARD_DESCRIPTION "When is this printed? From Midgard preview " []

muspelheim :: TreeNode =
  TreeNode "MUSPELHEIM" PlatformNode _MUSPELHEIM_DESCRIPTION "When is this printed? From muspelheim preview " []

mimirs_lake :: TreeNode = TreeNode "LAKE" PlatformNode _LAKE_DESCRIPTION "When is this printed? In lake preview" []

asgard :: TreeNode = TreeNode "ASGARD" PlatformNode "You've reached Asgard, home of the Gods!" "The bifrost lays beneath your eyes! This is your way to Asgard!" []

swartelheim :: TreeNode = TreeNode "SWARTELHEIM" PlatformNode "You've reached Swartelheim, land of the dwarves!" "You see some mountains in the distance. Could this be Swartelheim?" []

alvheim :: TreeNode = TreeNode "ALFHEIM" PlatformNode "You've reached Alvehim, land of the elves" "This forest looks deep and dangerous. This is not what you would expect Alfheim to look like" []

giantFight :: TreeNode = TreeNode "Giant House" (FightNode "Oh god, there's a fire giant in front of you. It is big." "The fire giant's dead body in front of you. It actually looks like the mountains surrounding. This creeps you out." 5 "hearts" (Obj "stone pickaxe")) "There's a giant stone house in front of you" "You can distinguish some kind of Giant House in the distance" []
