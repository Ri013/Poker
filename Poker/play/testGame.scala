package Poker.play

import Poker.Card
import Poker.cards.{Ace, Cards, Five, Four, Jack, King, Nine, Seven, Six, Three, Two}
import Poker.suit.{Club, Diamond, Heart, Spade, Suits}

import scala.util.Random

object testGame extends App{
  private val rng = Random
  var desk: List[(Suits, Cards)] = Card.desk()
    println(desk)

  val table: Table = Table( List((Heart, Four), (Heart, Five), ( Heart, Jack), (Diamond, Six), (Spade, Nine)))
  val hand1 = Hand(List((Diamond, Ace), (Club, Two)))
  val hand2 = Hand(List((Diamond, King), (Club, Three)))
  val player1 = Player("Nik", hand1, table )
  val player2 = Player("Jon", hand2, table )
  println(Card.sortCardOnSignificance(player1.cardsPlayer))
  println(Card.sortCardOnSignificance(player2.cardsPlayer))
  println(Card.compaire(player1, player2)(0).name)

}
