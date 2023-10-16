{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Game where

import Data.List
import Data.Set (fromList)
import Data.Tree
import GHC.Core.TyCon (newTyConEtadArity)
import System.Random ()
import Types
import Nodes

-- pre-generated game tree structure
gameTree :: Tree TreeNode = Node (root) [Node root [], Node midgard []]

-- we need smth like 'entry' which runs when we enter the node

runGame :: IO()
runGame = do
    putStrLn "Welcome to Binary Tree World.\n"
    putStrLn "You are at the root of an ancient binary tree."