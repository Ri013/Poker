package Poker.comb

import Poker.Card
import Poker.cards.{Cards, Two}
import Poker.suit.{Spade, Suits}

case object HighCard extends Combination {

   def getHighCard(cards: List[(Suits, Cards)]): Combination = {

      Card.sortCardOnSignificance(cards)(cards.length - 1)._2

   }

   override val priority: Int = 0

}