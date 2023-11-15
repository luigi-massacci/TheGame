module StartWorld where

import Levels
import Types

_START_WORLD :: IO (TreeZip Level)
_START_WORLD = do
  surtr_terminal <- mkTerminalNode surtr
  fafnir_terminal <- mkTerminalNode fafnir
  br1 <- mkBranch
  br2 <- mkBranch
  lake_terminal <- mkTerminalNode mimirs_lake
  helheim_terminal <- mkTerminalNode helheim
  asgard_terminal <- mkTerminalNode asgard
  vanheim_terminal <- mkTerminalNode vanheim
  feanor_terminal <- mkTerminalNode feanor
  br3 <- mkBranch
  br4 <- mkBranch
  return $
    TreeZip
      TOP
      ( Node
          roots
          ( Node
              muspelheim
              surtr_terminal
              ( Node
                  swartelheim
                  fafnir_terminal
                  br1
                  Leaf
                  Leaf
              )
              br2
              Leaf
          )
          lake_terminal
          helheim_terminal
          ( Node
              midgard
              asgard_terminal
              ( Node
                  alvheim
                  vanheim_terminal
                  feanor_terminal
                  br3
                  Leaf
              )
              br4
              Leaf
          )
      )
