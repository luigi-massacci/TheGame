module Constants where

_DEFAULT_LIFE_POINTS :: Int = 5

_DEFAULT_DAMAGE :: Int = 2

_HELP_MSG :: String =
  "\n\nWelcome! \n\
  \You can use the following commands: \n\
  \1) <help>: Displays this message\n\n\
  \Navigation:\n\
  \2) <go to PLACE_NAME>: Moves to PLACE_NAME. You will be told the possible places you can go to \n\
  \                       whenever you enter a node.\n\
  \3) <go back>: Return to the parent node\n\
  \4) <show map> : Prints the part of the tree you have currently explored\n\
  \5) <look> : TBD \n\n\
  \Fight: \n\
  \6) <rock> or <paper> or <scissors>: Plays the respective choice."

_ROOTS_DESCRIPTION :: String =
  "\n\nYou stand among the roots of Yggradsil, the world tree.\
  \Its bark is tougher than steel, and older than the universe.\n\
  \On and on it stretches upwards, farther than you could possibly ever see.\n\
  \High above you lie the great planes and all of their inhabitants. You feel very tiny indeed.\n\n\
  \Just barely, but you can distinguish MIDGARD, the mortal plane. There shepards tend to their flocks, \n\
  \farmers till their fields, fishermen mend their nets, and warriors sharpen their swords...\n\n\
  \From another branch, somewhat below Midgard, you feel a great burning heat, as if an hungy fire raged \n\
  \across the skies, wishing to consume all. Still, the world tree does not burn; its time has not yet come.\n\
  \This must be MUSPELHEIM, land of the fire giants. It is a place where even the gods fear to thread, and only a fool \n\
  \would wander unprepared. Yet are the greatest rewards not reaped by the greatest risks?\n\n\
  \A cold wind hits your shoulders: you turn and gaze upon HELHEIM, land of the dead. To your living body,\n\
  \it feels murky and incomprehensible. Do you fear death? \n\n\
  \Even further down than Helheim, at the very end of the tree (does the world tree ever end?) you seem to \n\
  \distinguish a clear LAKE, and a light shining brilliantly from its center. What could possibly be there?\n\n"

_MIDGARD_DESCRIPTION :: String =
  "\n\nYou have arrived in Midgard, the land of mortal men.\
  \Breathing comes easily here, and the sun shines upon your face. Overall, it is quite pleasant.\n\n\
  \The most magnificent sight of all lies before your eyes: the great rainbow bridge connecting Midgard to ASGARD,\n\
  \home of the Gods. Who knows what wanders lie beyond?\n\n\
  \A faint but intoxicating music reaches your ears. It is so beautiful as to be almost painful,\n\
  \and brings whispers of ALVHEIM, the fair realm of the elves.\n\n\
  \As you gaze downward on the land you see a host of warriors preparing for battle; they are laying siege to a CITY\n\
  \of colossal size. Its walls are so tall they seem to scrape the sky, and the rooftops of its many palaces shine\n\
  \as if made of gold. There must be great riches to be claimed and glory to be won."

_MUSPELHEIM_DESCRIPTION :: String =
  "\n\nYou've reached the borders of Muspelheim, land of the fire giants. The ground is covered by a thick layer of ash,\n\
  \and scarred by rivers of lava. You struggle to breathe, and the sky is blackened by the smoke from the many raging fires.\n\
  \Here and there you see giants clothed in flames going about their way.\n\
  \At the top of a mountain, sitting on a trone of black stone is SURTR, king of the giants."

_SURTR_FIGHT :: String = "\n\nHe has a shining sword laying on his lap. He looks at you and sneers."

_SURTR_VICTORY :: String = "TBD victory"

_LAKE_DESCRIPTION :: String =
  "\n\nHere you are, at the furthermost corner of the Tree anybody has ever explored. Even the Allfather never\n\
  \never went beyond this point. The water in the pool is impossibly clear, and yet startngly blue.\n\
  \Even from here you cannot tell what the source of the light at the center is.\n\
  \Perhaps you should wade in and take a closer look?"