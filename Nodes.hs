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
midgard :: TreeNode = TreeNode "Midgard" PlatformNode "You've reached Asgard, home of the gods!"
-- add mode pre-generated nodes here