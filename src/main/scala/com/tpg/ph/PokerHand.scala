package com.tpg.ph

sealed abstract class PokerHand(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card)
  extends Hand(Seq(card1, card2, card3, card4, card5)) {
}

case class StraightFlush(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends PokerHand(card1, card2, card3, card4, card5)

object StraightFlush {
  def apply(cards: Seq[Card]): Option[StraightFlush] = {
    IsStraightFlush(cards) match {
      case true => Option(StraightFlush(cards(0), cards(1), cards(2), cards(3), cards(4)))
      case false => None
    }
  }
}

object IsStraightFlush {
  def apply(cards: Seq[Card]): Boolean = {
    cards.isEmpty match {
      case true => false
      case false => {
        val suit = cards(0).suit
        val count = cards.count(c => c.suit == suit)
        val cardValues = cards.sortWith(_ < _).map(c => c.value)
        val range = cardValues(0) to cardValues(cards.size - 1)

        count == 5 && range == cardValues
      }
    }
  }
}

case class HighCard(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends PokerHand(card1, card2, card3, card4, card5)

object PokerHand {
  def apply(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card): Option[PokerHand] = {
    val cards = Seq(card1, card2, card3, card4, card5)
    val suits = cards.groupBy(c => c.suit)
    suits.size == 1 match {
      case true => {
        val sorted = cards.sortWith(_ < _)
        val values = sorted.map(c => c.value)
        val range = values(0) to values(4)

        range == values match {
          case true => Option(StraightFlush(card1, card2, card3, card4, card5))
          case false => None
        }
      }

      case false => Option(HighCard(card1, card2, card3, card4, card5))
    }
  }
}
