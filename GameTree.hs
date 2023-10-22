{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Game where

import Data.List
import Data.Set (fromList)
--import Data.Tree
import GHC.Core.TyCon (newTyConEtadArity)
import Nodes
import System.Random ()
import Types
import Parser

-- pre-generated game tree structure
endNode :: a -> QuadTree a
endNode x = Node x Leaf Leaf Leaf Leaf

gameTree :: TreeZip TreeNode = TreeZip TOP (Node helheim Leaf Leaf Leaf (Node root (endNode muspelheim) Leaf Leaf (Node midgard (endNode swartelheim) (endNode asgard) (endNode alvheim) Leaf)))


-- we need smth like 'entry' which runs when we enter the node
runGame :: IO()
runGame = do
    putStrLn "Welcome to Binary Tree World.\n"
    putStrLn "You are at the root of an ancient binary tree."
    