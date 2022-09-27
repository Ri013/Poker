package Poker.comb

import Poker.Card
import Poker.cards.Cards
import Poker.suit.Suits

case object TwoPair extends Combination {

   override val priority: Int = 16

   override  def thisIsIt(cards: List[(Suits, Cards)]): Boolean = {

      def examination(n: Int)(counter: Int)(list: List[(Suits, Cards)]): Boolean = {
         if (n < list.length) {
            list(n) match {
               case (a, b) if (n != 0 && list(n - 1)._2.priority == b.priority && counter == 1) => true
               case (a, b) if (n != 0 && list(n - 1)._2.priority == b.priority) => examination(n - 2)(counter + 1)(list)
               case (a, b) if (n == 0 ) => false
               case (a, b) => examination(n -1)(counter)(list)
               case _ => false
            }
         }
         else false
      }

      examination(cards.length-1)(0)(Card.sortCardOnSignificance(cards))
   }

   override def getCombination(cards: List[(Suits, Cards)]): List[(Suits, Cards)] = {
      def examination(n: Int,counter: Int)(l:List[(Suits, Cards)])(list: List[(Suits, Cards)]): List[(Suits, Cards)] = {
         if (n < list.length) {

            list(n) match {
               case (a, b) if (n != 0 && list(n - 1)._2.priority == b.priority && counter == 1) => l.appendedAll(list.slice(n - 1,n + 1)).reverse
               case (a, b) if (n != 0 && list(n - 1)._2.priority == b.priority) => examination(n - 2,counter + 1)(list.slice(n - 1, n + 1))(list)
               case (a, b) if (n == 0) => Nil
               case (a, b) => examination(n - 1, counter)(l)(list)
               case _ => Nil
            }
         }
         else Nil
      }
      examination(cards.length-1,0)(Nil)(Card.sortCardOnSignificance(cards))
   }
}
