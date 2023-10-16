{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Game where

import Data.List
import Data.Set (fromList)
import Data.Tree
import GHC.Core.TyCon (newTyConEtadArity)
import Nodes
import System.Random ()
import Types

-- pre-generated game tree structure
gameTree :: Tree TreeNode = Node (root) []