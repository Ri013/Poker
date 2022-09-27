package Poker.suit

sealed trait Suits {
  val numberSuit:Int
}

case object Diamond extends Suits {
  override val numberSuit: Int = 1
}

case object Heart extends Suits {
  override val numberSuit: Int = 2
}

case object Club extends Suits {
  override val numberSuit: Int = 3
}

case object Spade extends Suits {
  override val numberSuit: Int = 4
}



