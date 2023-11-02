{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Types where

import Data.List
import Data.Set (fromList)
import GHC.Core.TyCon (newTyConEtadArity)
import System.Random

newtype Object = Obj String
  deriving (Show, Eq)

data Player = Player {life :: Int, attack :: Int, inventory :: [Object]}
  deriving (Show)

data NodeType = FightNode {fightText :: String, defeatedText :: String, lifepoints :: Int, lifeName :: String, object :: Object} | PlatformNode | RandomNode
  deriving (Show)

data TreeNode = TreeNode
  { name :: String,
    nodetype :: NodeType,
    msg :: String,
    previewmsg :: String, -- What will be shown from the parent node
    necessary_items :: [Object]
    -- parent :: Maybe (Tree TreeNode) -- change on move function -- zippers later
  }
  deriving (Show)

data QuadTree a = Leaf | Node {label :: a, ll :: (QuadTree a), l :: (QuadTree a), r :: (QuadTree a), rr :: (QuadTree a)}
  deriving (Show)

-- | A Node with only leaves as children
mkTerminalNode :: a -> QuadTree a
mkTerminalNode x = Types.Node x Leaf Leaf Leaf Leaf

data Context a = TOP | LL a (Context a) (QuadTree a) (QuadTree a) (QuadTree a) | L a (QuadTree a) (Context a) (QuadTree a) (QuadTree a) | R a (QuadTree a) (QuadTree a) (Context a) (QuadTree a) | RR a (QuadTree a) (QuadTree a) (QuadTree a) (Context a)
  deriving (Show)

data TreeZip a = TreeZip {context :: Context a, tree :: QuadTree a}
  deriving (Show)

data GameInstance = Game {gamezip :: TreeZip TreeNode, player :: Player}
  deriving (Show)

data AttackType = Rock | Paper | Scissors
  deriving (Show, Eq)

data Action = Attack AttackType | Move String | Look | Help | ShowMap
