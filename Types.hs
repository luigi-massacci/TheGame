{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Types where

import Data.List
import Data.Set (fromList)
import Data.Tree
import GHC.Core.TyCon (newTyConEtadArity)
import System.Random

newtype Object = Obj String
  deriving (Show)

data Player = Player {life :: Int, inventory :: [Object]}

data NodeType = FightNode {fightText :: String, defeatedText :: String, lifepoints :: Int, object :: Object} | PlatformNode | RandomNode
  deriving (Show)

data TreeNode = TreeNode
  { name :: String,
    nodetype :: NodeType,
    msg :: String
    -- parent :: Maybe (Tree TreeNode) -- change on move function -- zippers later
  }
  deriving (Show)

data GameInstance = Game {tree :: Tree TreeNode, player :: Player}

data ActionType = Attack | Move | Look

data Action = Act ActionType String
