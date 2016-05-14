package com.tpg.ph

import com.tpg.ph.StraightFlush.isStraightFlush

import scala.annotation.tailrec

sealed abstract class PokerHand(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card)
  extends Hand(Seq(card1, card2, card3, card4, card5)) {
  def rank(that: PokerHand): Option[PokerHand]
}

object PokerHand {
  def apply(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card): Option[PokerHand] = {
    val cards = Seq(card1, card2, card3, card4, card5)
    val hand1 = StraightFlush(cards)

    hand1.isEmpty match {
      case true => HighCard(cards)
      case false => hand1
    }
  }
}


case class StraightFlush(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card)
  extends PokerHand(card1, card2, card3, card4, card5) {
  override def rank(that: PokerHand): Option[PokerHand] = {
    isStraightFlush(that.cards) match {
      case true => rank(0, that)
      case false => Option(this)
    }
  }

  private def rank(index: Int, that: PokerHand): Option[PokerHand] = {
    this == that match {
      case true => None
      case false => this > that match {
        case true => Option(this)
        case false => Option(that)
      }
    }
  }

  override def canEqual(obj: Any): Boolean = obj.isInstanceOf[StraightFlush]

  override def equals(any: Any): Boolean = any match {
    case that: StraightFlush => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def hashCode: Int = 31 + cards.hashCode
}

object StraightFlush {
  def apply(cards: Seq[Card]): Option[StraightFlush] = {
    isStraightFlush(cards) match {
      case true => Option(new StraightFlush(cards.head, cards(1), cards(2), cards(3), cards(4)))
      case false => None
    }
  }

  def isStraightFlush(cards: Seq[Card]): Boolean = {
    cards.isEmpty match {
      case true => false
      case false => {
        val suit = cards.head.suit
        val count = cards.count(c => c.suit == suit)
        val sortedCards = cards.sortWith(_.value < _.value)
        val range = sortedCards.head.value to sortedCards.last.value

        count == 5 && range == sortedCards.map {c => c.value}
      }
    }
  }
}


case class HighCard(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends PokerHand(card1, card2, card3, card4, card5) {
  override def rank(that: PokerHand): Option[PokerHand] = {
    this.cards.size == that.cards.size match {
      case true => highestValueCard(that, 0)
      case false => None
    }
  }

  @tailrec
  private def highestValueCard(that: PokerHand, index: Int): Option[PokerHand] = {
    index == this.cards.size match {
      case true => None
      case false => {
        this.highestValuedCard(index) == that.highestValuedCard(index) match {
          case true => highestValueCard(that, index+1)
          case false => Option(Seq(this, that).sortWith(_.highestValuedCard(index).get > _.highestValuedCard(index).get).head)
        }
      }
    }
  }
}

object HighCard {
  def apply(cards: Seq[Card]): Option[PokerHand] = {
    cards.size == 5 match {
      case true => Option(HighCard(cards.head, cards(1), cards(2), cards(3), cards(4)))
      case false => None
    }
  }
}