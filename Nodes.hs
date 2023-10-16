{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

import Data.List
import Data.Set (fromList)
import Data.Tree
import GHC.Core.TyCon (newTyConEtadArity)
import System.Random
import Types

midgard :: TreeNode = TreeNode "Midgard" PlatformNode "You've reached Asgard, home of the gods!"
