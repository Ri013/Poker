package Poker


import Poker.cards.{Ace, Cards, Eight, Five, Four, Jack, King, Nine, Queen, Seven, Six, Ten, Three, Two}
import Poker.comb.{Care, Combination, Flush, FullHouse, HighCard, Pair, RoyalFlush, Straight, StraightFlush, Troika, TwoPair}
import Poker.play.Player
import Poker.suit.{Club, Diamond, Heart, Spade, Suits}

case class Card () {
}

object Card {
  def getPriortyHighCard(comb: Combination, player: Player): Int = {
    comb.getCombination(player.cardsPlayer)(comb.getCombination(player.cardsPlayer).length)._2.priority
  }
  def desk(): List[(Suits, Cards)] = {
    suits.flatMap(a => {
      significance.foldRight(Nil: List[(Suits, Cards)])((x, y) => (a, x) :: y)
    })
  }
  def moreLess(numberComb: Int)(player1: Player, player2: Player): List[Player] = {
    numberComb match {
      case 23 => List (player1,player2)
      case 22 => highCard(StraightFlush,player1, player2)
      case 21 => highCard(Care,player1, player2)
      case 20 => {
        if (highCard(Troika, player1, player2).length == 1)
          (highCard(Troika, player1, player2))
        else comparisonPair(player1, player2)
      }
      case 19 => highCard(Flush, player1, player2)
      case 18 => highCard(Straight, player1, player2)
      case 17 => highCard(Troika, player1, player2)
      case 16 => comparisonTwoPair(player1, player2)
      case 15 => comparisonPair(player1, player2)
      case a if (a <= 14) => comparisonHand(player1, player2)
      }
    }

  def comparisonHand (player1: Player, player2: Player): List[Player] = {
     if (player1.hand.cardsHand(1)._2 > player2.hand.cardsHand(1)._2)
      List(player1)
    else if (player1.hand.cardsHand(1)._2 < player2.hand.cardsHand(1)._2)
      List(player2)
    else if (player1.hand.cardsHand(0)._2 > player2.hand.cardsHand(0)._2)
      List(player1)
    else if (player1.hand.cardsHand(0)._2 < player2.hand.cardsHand(0)._2)
      List(player2)
    else List(player1, player2)
  }

 def comparisonPair  (player1: Player, player2: Player): List[Player] = {
   val combPl1 = Pair.getCombination(player1.cardsPlayer)
   val combPl2 = Pair.getCombination(player2.cardsPlayer)
   if (combPl1(1)._2.priority > combPl2(1)._2.priority)
     List(player1)
   else if (combPl1(1)._2.priority < combPl2(1)._2.priority)
     List(player2)
     else comparisonHand (player1, player2)
 }

  def comparisonTwoPair (player1: Player, player2: Player): List[Player] = {
    val combPl1 = TwoPair.getCombination(player1.cardsPlayer)
    val combPl2 = TwoPair.getCombination(player2.cardsPlayer)
    if(combPl1(3)._2.priority > combPl2(3)._2.priority)
      List(player1)
   else if (combPl1(3)._2 < combPl2(3)._2)
      List(player2)
    else if (combPl1(1)._2 < combPl2(1)._2)
      List(player2)
    else if (combPl1(1)._2 > combPl2(1)._2)
      List(player1)
      else List(player1, player2)
    }

    def highCard (comb:Combination, player1: Player, player2: Player): List[Player] ={
      if (Card.getPriortyHighCard(comb,player1) > Card.getPriortyHighCard(comb,player2))
        List(player1)
        else if (Card.getPriortyHighCard(comb,player1) < Card.getPriortyHighCard(comb,player2))
        List(player2)
        else  List(player1, player2)
    }

  def compaire(player1: Player, player2: Player): List[Player] = {
    println(player1.bestCombination().priority)
    println(player2.bestCombination().priority)
    (player1.bestCombination().priority, player2.bestCombination().priority) match {
      case (a, b) if (a > b) => List(player1)
      case (a, b) if (a < b) => List(player2)
      case (a, b) if (a == b) => moreLess(a)(player1, player2)
    }
  }
  def sortCardOnSignificance(l: List[(Suits, Cards)]): List[(Suits, Cards)] = {
    l.sortWith((x, y) => x._2 < y._2)
  }

  def sortCardOnSuit(l: List[(Suits, Cards)]): List[(Suits, Cards)] = {
    (l.sortWith((x, y) => x._1.numberSuit < y._1.numberSuit )).sortWith((x,y) => if ( x._1.numberSuit == y._1.numberSuit) x._2 < y._2 else false)
  }

 private val significance = List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)
 private val suits = List(Diamond, Heart, Club, Spade)

}


