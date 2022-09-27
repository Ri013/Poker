package Poker.play

import Poker.comb.{Care, Combination, Flush, FullHouse, HighCard, Pair, RoyalFlush, Straight, StraightFlush, Troika, TwoPair}

case class Player(val name: String,val hand: Hand, val tadle: Table)  {

  val cardsPlayer = hand.cardsHand.appendedAll(tadle.cardsTable)

  def bestCombination(): Combination = {
    if (RoyalFlush.thisIsIt(this.cardsPlayer)) {
      RoyalFlush
    } else if (StraightFlush.thisIsIt(this.cardsPlayer))
      StraightFlush
    else if (Care.thisIsIt(this.cardsPlayer))
      Care
    else if (FullHouse.thisIsIt(this.cardsPlayer))
      FullHouse
    else if (Flush.thisIsIt(this.cardsPlayer))
      Flush
    else if (Straight.thisIsIt(this.cardsPlayer))
      Straight
    else if (Troika.thisIsIt(this.cardsPlayer))
      Troika
    else if (TwoPair.thisIsIt(this.cardsPlayer))
      TwoPair
    else if (Pair.thisIsIt(this.cardsPlayer))
      Pair
    else
      HighCard.getHighCard(this.cardsPlayer)
  }
}

case object Player{

}


