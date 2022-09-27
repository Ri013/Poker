package Poker.comb

import Poker.cards.Cards
import Poker.suit.Suits

trait Combination {
  def comparison(c2: Combination): Int = {
    if (this.priority > c2.priority)
      1
    else if (this.priority < c2.priority)
      -1
    else 0
  }

  def > (c2: Combination): Boolean ={
    comparison(c2) == 1
  }

  def < (c2: Combination): Boolean = {
    comparison(c2) == -1
  }


    def thisIsIt(cards: List[(Suits, Cards)]): Boolean = false

    def getCombination(cards: List[(Suits, Cards)]): List[(Suits, Cards)] = Nil

    val priority: Int
}

