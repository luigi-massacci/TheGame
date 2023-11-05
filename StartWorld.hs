module StartWorld where

import Levels
import Types

_START_WORLD :: TreeZip Level =
  TreeZip
    TOP
    ( Node
        roots
        ( Node
            muspelheim
            (mkTerminalNode surtr)
            ( Node
                swartelheim
                (mkTerminalNode fafnir)
                mkBranch
                Leaf
                Leaf
            )
            mkBranch
            Leaf
        )
        (mkTerminalNode mimirs_lake)
        (mkTerminalNode helheim)
        ( Node
            midgard
            (mkTerminalNode asgard)
            ( Node
                alvheim
                (mkTerminalNode vanheim)
                (mkTerminalNode feanor)
                mkBranch
                Leaf
            )
            mkBranch
            Leaf
        )
    )
