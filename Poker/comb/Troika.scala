package Poker.comb

import Poker.Card
import Poker.cards.Cards
import Poker.suit.Suits


case object Troika  extends Combination {

  override val priority: Int = 17

  override def thisIsIt(cards: List[(Suits, Cards)]): Boolean ={

   def examination(n: Int)(list: List[(Suits, Cards)]): Boolean = {
      if (n < list.length - 2) {
         list(n) match {
            case (a, b) if (n <= list.length - 3 && list(n + 1)._2.priority == b.priority && list(n + 2)._2.priority == b.priority) => true
            case (a, b) => examination(n + 1)(list)
            case _ => false
         }
      }
      else false
   }


   examination(0)(Card.sortCardOnSignificance(cards))
}

  override  def getCombination(cards: List[(Suits, Cards)]): List[(Suits, Cards)] = {
    def examination(n: Int)(list: List[(Suits, Cards)]): List[(Suits, Cards)] = {
      if (n < list.length) {

        list(n) match {
          case (a, b) if (n <= list.length - 3 && list(n + 1)._2.priority == b.priority && list(n + 2)._2.priority == b.priority) => list.slice(n, n+3)
          case (a, b) => examination(n + 1)(list)
          case _ => Nil
        }

      }
      else Nil
    }

    examination(0)(Card.sortCardOnSignificance(cards))
  }

}
