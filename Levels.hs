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
  Level "HELHEIM" Platform "You've reached Helheim, land of the dead!" "" [] False

roots :: Level =
  Level "ROOTS" Platform _ROOTS_DESCRIPTION "" [] False

midgard :: Level =
  Level "MIDGARD" Platform _MIDGARD_DESCRIPTION "" [] False

muspelheim :: Level =
  Level "MUSPELHEIM" Platform _MUSPELHEIM_DESCRIPTION "" [] False

mimirs_lake :: Level = Level "LAKE" Platform _LAKE_DESCRIPTION "" [] False

asgard :: Level = Level "ASGARD" Platform "\nYou've reached Asgard, home of the Gods!" _ASGARD_LOCK ["sword", "cape", "armor"] False

swartelheim :: Level = Level "SWARTELHEIM" Platform _SWARTELEIM_DESCRIPTION "" [] False

fafnir :: Level = Level "CAVE" (Fight _CAVE_VICTORY_TEXT 3 "armor") _CAVE_FIGHT_TEXT "" [] False

alvheim :: Level = Level "ALVHEIM" Platform _ALVEHIM_DESCRIPTION "" [] False

feanor :: Level = Level "FEANOR" (Fight _FEANOR_VICTORY 2 "cape") _FEANOR_FIGHT "" [] False

vanheim :: Level = Level "VANHEIM" Platform _VANHEIM_DESCRIPTION _VANHEIM_LOCK ["cape"] False

surtr :: Level = Level "SURTR" (Fight _SURTR_VICTORY 5 "sword") _SURTR_FIGHT "" [] False
