{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Types where

import Data.List
import Data.Set (fromList)
import Data.Tree
import GHC.Core.TyCon (newTyConEtadArity)
import System.Random

data NodeType = FightNode | PlatformNode | RandomNode
  deriving (Show)

data TreeNode = TreeNode
  { name :: String,
    nodetype :: NodeType,
    msg :: String
    -- parent :: Maybe (Tree TreeNode) -- change on move function -- zippers later
  }
  deriving (Show)