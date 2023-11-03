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
                Leaf
                Leaf
                Leaf
            )
            Leaf
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
                Leaf
                Leaf
            )
            Leaf
            Leaf
        )
    )
