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



data QuadTree a = Leaf | Node {label : a, ll : (QuadTree a), l : (QuadTree a), r : (QuadTree a), rr : (QuadTree a)}

data Context a = TOP | LL a (Context a) (QuadTree a) (QuadTree a) (QuadTree a) | L a (QuadTree a) (Context a) (QuadTree a) (QuadTree a) | R a (QuadTree a) (QuadTree a) (Context a) (QuadTree a) | RR a (QuadTree a) (QuadTree a) (QuadTree a) (Context a)

data TreeZip a = TreeZip {context : Context a, tree : QuadTree a}
  deriving (Show)

data GameInstance = Game {tree :: TreeZip TreeNode, player :: Player}

data AttackType = Rock | Paper | Scissors

data Action = Attack AttackType | Move String | Look
