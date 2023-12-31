{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Types where

import Data.List
import Data.Set (fromList)
import GHC.Core.TyCon (newTyConEtadArity)
import LevelDescriptions
import RandomTexts
import System.Random

type Object = String

-- | Player related data types
data Player = Player {lifePoints :: Int, attackDamage :: Int, inventory :: [Object]}
  deriving (Show)

data AttackType = Rock | Paper | Scissors
  deriving (Show, Eq)

data Action = Attack AttackType | Move String | Look | Help | ShowMap

-- | Story/World related data types
data LevelType
  = Fight
      { victoryText :: String,
        enemyLifePoints :: Int,
        item :: Object
      }
  | Platform
  | Random
  deriving (Show)

data Level = Level
  { name :: String,
    leveltype :: LevelType,
    description :: String,
    lockMessage :: String,
    necessaryItems :: [Object],
    visited :: Bool
  }
  deriving (Show)

-- | Tree setup data types
data QuadTree a = Leaf | Node {root :: a, ll :: QuadTree a, l :: QuadTree a, r :: QuadTree a, rr :: QuadTree a}
  deriving (Show)

-- | A Node with only leaves and a branch as its children (branch later extends as infinity tree).
mkTerminalNode :: Level -> IO (QuadTree Level)
mkTerminalNode x = do
  branch <- mkBranch
  return (Node x branch Leaf Leaf Leaf)

-- | Branch - entry point of the infinite tree. Has 1 upper and 1 lower (random) branches as children.
mkBranch :: IO (QuadTree Level)
mkBranch = do
  upper_branch <- mkRandomTree "UPPER BRANCH"
  lower_branch <- mkRandomTree "LOWER BRANCH"
  return (Node (Level "BRANCH" Platform _BRANCH_TEXT "" [] False) upper_branch lower_branch Leaf Leaf)

-- | Given level, adds two random nodes (upper and lower branches). Used to extend a node.
mkTripleTree :: Level -> IO (QuadTree Level)
mkTripleTree lvl = do
  rt1 <- mkRandomTree "UPPER BRANCH"
  rt2 <- mkRandomTree "LOWER BRANCH"
  return (Node lvl rt1 rt2 Leaf Leaf)

flipCoin :: IO Bool
flipCoin = do
  randomValue <- randomIO :: IO Bool -- Generate a random boolean value (True/False)
  return randomValue

-- | Generates a random platform or fight level
mkRandomTree :: String -> IO (QuadTree Level)
mkRandomTree name_current = do
  coin_flip <- flipCoin
  item_index <- randomRIO (0, 2)
  strength <- randomRIO (1, 10)
  text_index <- randomRIO (0, length texts - 1)
  let text = texts !! text_index
  let item = ["sword", "cape", "armor"] !! item_index
  let root_level =
        if coin_flip
          then Level name_current Platform (name_current ++ ":\n" ++ text) "" [] False
          else Level name_current (Fight "Victorious in battle, thou hast conquered! Hail, warrior, thy triumph resounds through the nine realms. Skål!" strength item) "Amidst the ancient forest, you faced a shadowy creature on Yggdrasil's branch, a battle of mortal against the mystical." "" [] False
  return (Node root_level Leaf Leaf Leaf Leaf)

data Context a = TOP | LL a (Context a) (QuadTree a) (QuadTree a) (QuadTree a) | L a (QuadTree a) (Context a) (QuadTree a) (QuadTree a) | R a (QuadTree a) (QuadTree a) (Context a) (QuadTree a) | RR a (QuadTree a) (QuadTree a) (QuadTree a) (Context a)
  deriving (Show)

data TreeZip a = TreeZip {context :: Context a, tree :: QuadTree a}
  deriving (Show)

data GameInstance = Game {gamezip :: TreeZip Level, player :: Player}
  deriving (Show)
