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

root :: TreeNode = TreeNode "Root" PlatformNode "You are in the root, you can go somewhere!"

midgard :: TreeNode = TreeNode "Midgard" PlatformNode "You've reached Midgard, the mortal plane!"

asgard :: TreeNode = TreeNode "Asgard" PlatformNode "You've reached Asgard, home of the Gods!"

swartelheim :: TreeNode = TreeNode "Swartelheim" PlatformNode "You've reached Swartelheim, land of the dwarves!"

muspelheim :: TreeNode = TreeNode "Muspelheim" PlatformNode "You've reached Muspelheim, land of the fire giants!"

helheim :: TreeNode = TreeNode "Helheim" PlatformNode "You've reached Helheim, land of the dead!"

alvheim :: TreeNode = TreeNode "Alvheim" PlatformNode "You've reached Alvehim, land of the elves"

yggradasil :: Tree TreeNode =
  Node
    root
    [ Node midgard [Node asgard [], Node alvheim []],
      Node swartelheim [],
      Node muspelheim [],
      Node helheim []
    ]