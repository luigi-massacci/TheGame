{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Nodes where

import Constants
import Data.List
import Data.Set (fromList)
import GHC.Core.TyCon (newTyConEtadArity)
import System.Random
import Types

-- Initial nodes

helheim :: Level =
  Level "HELHEIM" Platform "You've reached Helheim, land of the dead!" []

roots :: Level =
  Level "ROOTS" Platform _ROOTS_DESCRIPTION []

midgard :: Level =
  Level "MIDGARD" Platform _MIDGARD_DESCRIPTION []

muspelheim :: Level =
  Level "MUSPELHEIM" Platform _MUSPELHEIM_DESCRIPTION []

mimirs_lake :: Level = Level "LAKE" Platform _LAKE_DESCRIPTION []

asgard :: Level = Level "ASGARD" Platform "You've reached Asgard, home of the Gods!" []

swartelheim :: Level = Level "SWARTELHEIM" Platform "You've reached Swartelheim, land of the dwarves!" []

alvheim :: Level = Level "ALFHEIM" Platform "You've reached Alvehim, land of the elves" []

giantFight :: Level = Level "Giant House" (Fight "TBD1" "TBD2" 5 "TBD3" ("TBD4")) "TBD5" []
