{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Levels where

import Data.List
import Data.Set (fromList)
import GHC.Core.TyCon (newTyConEtadArity)
import LevelDescriptions
import System.Random
import Types

-- Initial nodes

helheim :: Level =
  Level "HELHEIM" Platform "You've reached Helheim, land of the dead!" "" []

roots :: Level =
  Level "ROOTS" Platform _ROOTS_DESCRIPTION "" []

midgard :: Level =
  Level "MIDGARD" Platform _MIDGARD_DESCRIPTION "" []

muspelheim :: Level =
  Level "MUSPELHEIM" Platform _MUSPELHEIM_DESCRIPTION "" []

mimirs_lake :: Level = Level "LAKE" Platform _LAKE_DESCRIPTION "" []

asgard :: Level = Level "ASGARD" Platform "You've reached Asgard, home of the Gods!" _ASGARD_LOCK ["sword", "cape", "armor"]

swartelheim :: Level = Level "SWARTELHEIM" Platform "You've reached Swartelheim, land of the dwarves!" "" []

fafnir :: Level = Level "FAFNIR" (Fight _FAFNIR_VICTORY 3 "armor") _FAFNIR_FIGHT "" []

alvheim :: Level = Level "ALFHEIM" Platform "You've reached Alvehim, land of the elves" "" []

feanor :: Level = Level "FEANOR" (Fight _FEANOR_VICTORY 2 "cape") _FEANOR_FIGHT "" []

vanheim :: Level = Level "VANHEIM" Platform "You've reached Vanheim, land of the Vanir" _VANHEIM_LOCK ["cape"]

surtr :: Level = Level "SURTR" (Fight _SURTR_VICTORY 5 "sword") _SURTR_FIGHT "" []
