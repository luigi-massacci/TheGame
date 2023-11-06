# TheGame
To run two options:

Interpreter:

$> ghci GameMain

ghci> runGame

Compiler:

ghc GameMain


General structure of the game:
Theme is Norse mythology, climbing the Yggdrasil tree.
You are navigating the infinite tree, where some of the branches are pre-generated and other branches get randomly generated on demand (they get generated when player accesses nodes that reach those branches). Each node is a level of either type Platform or Fight. In Platform levels you can find description of the place, in fight nodes you get challanged to rock paper scissors fight. Each fighter has life points which is displayed before the fight starts. If you lose the fight you start game over from the beginning. Defeating opponent gives you reward which unlocks some levels, which are previously locked. For all useful commands type <help> command. Your goal is to reach ASGARD. Good luck!