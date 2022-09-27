package Poker.comb

import Poker.Card
import Poker.cards.Cards
import Poker.suit.Suits


case object Care extends Combination {
   override val priority: Int = 21

   override def thisIsIt(cards: List[(Suits, Cards)]): Boolean = {
      def examination(n: Int)(counter: Int)(list: List[(Suits, Cards)]): Boolean = {
         if (n < list.length ) {
            list(n) match {
               case (a, b) if ( counter == 3 && n <= list.length - 1 && b.priority == list(n - 1)._2.priority) => true
               case (a, b) if (n != list.length - 1 && list(n + 1)._2.priority == b.priority) => examination(n + 1)(counter + 1)(list)
               case (a, b) => examination(n + 1)(0)(list)
               case _ => false
            }
         }
         else false
      }
         examination(0)(0)(Card.sortCardOnSignificance(cards))
      }
   override def getCombination(cards: List[(Suits, Cards)]): List[(Suits, Cards)] = {
      def examination(n: Int)(counter: Int)(list: List[(Suits, Cards)]): List[(Suits, Cards)] = {
         if (n < list.length) {
            list(n) match {
               case (a, b) if (counter == 3 && n <= list.length - 1 && b.priority == list(n - 1)._2.priority) => list.slice(n - 3, n + 1)
               case (a, b) if (n != list.length - 1 && list(n + 1)._2.priority == b.priority) => examination(n + 1)(counter + 1)(list)
               case (a, b) => examination(n + 1)(0)(list)
               case _ => Nil
            }
         }
         else Nil
      }

      examination(0)(0)(Card.sortCardOnSignificance(cards))
   }

}
