package Poker.cards

import Poker.comb.Combination

sealed trait Cards extends Combination {
  val priority: Int
}

 object Two extends Cards {
  override val priority: Int = 2
}

 object Three extends Cards {
  override val priority: Int = 3
}

 object Four extends Cards {
  override val priority: Int = 4
}

 object Five extends Cards {
  override val priority: Int = 5
}

 object Six extends Cards {
  override val priority: Int = 6
}

 object Seven extends Cards {
  override val priority: Int = 7
}

 object Eight extends Cards {
  override val priority: Int = 8
}

 object Nine extends Cards {
  override val priority: Int = 9
}

 object Ten extends Cards {
  override val priority: Int = 10
}

 object Jack extends Cards {
  override val priority: Int = 11
}

 object Queen extends Cards {
  override val priority: Int = 12
}

 object King extends Cards {
  override val priority: Int = 13
}

 object Ace extends Cards {
  override val priority: Int = 14
}








