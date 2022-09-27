package Poker.comb

import Poker.Card
import Poker.cards.{Ace, Cards}
import Poker.suit.Suits

case object RoyalFlush extends Combination {
  override val priority: Int = 23
  override  def thisIsIt(cards: List[(Suits, Cards)]): Boolean = {

     def examination(n: Int)(counter: Int)(list: List[(Suits, Cards)]): Boolean = {
       if (n < list.length) {

         list(n) match {
           case (a, b) if (counter == 4 && list(n - 1)._1.numberSuit == a.numberSuit && list(n - 1)._2.priority + 1 == b.priority) => true
           case (a, b) if (n != list.length - 1 && list(n + 1)._1.numberSuit == a.numberSuit
             && list(n + 1)._2.priority == b.priority + 1 && b.priority == 10+counter) => examination(n + 1)(counter + 1)(list)
           case (a, b) => examination(n + 1)(0)(list)
           case _ => false
         }
       }
       else false
     }
     examination(0)(0)(Card.sortCardOnSuit(cards))
   }



}
