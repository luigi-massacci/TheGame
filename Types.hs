{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Types where

import Data.List
import Data.Set (fromList)
import GHC.Core.TyCon (newTyConEtadArity)
import System.Random

type Object = String

data Player = Player {life :: Int, attack_damage :: Int, inventory :: [Object]}
  deriving (Show)

data LevelType = Fight {fightText :: String, defeatedText :: String, lifepoints :: Int, lifeName :: String, object :: Object} | Platform | Random
  deriving (Show)

data Level = Level
  { name :: String,
    leveltype :: LevelType,
    description :: String,
    necessary_items :: [Object]
  }
  deriving (Show)

data QuadTree a = Leaf | Node {root :: a, ll :: QuadTree a, l :: QuadTree a, r :: QuadTree a, rr :: QuadTree a}
  deriving (Show)

-- | A Node with only leaves as children
mkTerminalNode :: a -> QuadTree a
mkTerminalNode x = Types.Node x Leaf Leaf Leaf Leaf

data Context a = TOP | LL a (Context a) (QuadTree a) (QuadTree a) (QuadTree a) | L a (QuadTree a) (Context a) (QuadTree a) (QuadTree a) | R a (QuadTree a) (QuadTree a) (Context a) (QuadTree a) | RR a (QuadTree a) (QuadTree a) (QuadTree a) (Context a)
  deriving (Show)

data TreeZip a = TreeZip {context :: Context a, tree :: QuadTree a}
  deriving (Show)

data GameInstance = Game {gamezip :: TreeZip Level, player :: Player}
  deriving (Show)

data AttackType = Rock | Paper | Scissors
  deriving (Show, Eq)

data Action = Attack AttackType | Move String | Look | Help | ShowMap
