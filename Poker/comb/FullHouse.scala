package Poker.comb

import Poker.cards.Cards
import Poker.suit.Suits

case object FullHouse  extends Combination {
  override val priority: Int = 20
  override def thisIsIt(cards: List[(Suits, Cards)]): Boolean = {
      if(Troika.thisIsIt(cards) && Pair.thisIsIt(cards))
         true
      else false
   }

  override def getCombination(cards: List[(Suits, Cards)]): List[(Suits, Cards)] = {
    Troika.getCombination(cards).appendedAll(Pair.getCombination(cards))
  }

}
