package com.tpg.ph

import com.tpg.ph.FourOfAKind.isFourOfAKind
import com.tpg.ph.StraightFlush.isStraightFlush
import com.tpg.ph.FullHouse.isFullHouse

import scala.annotation.tailrec

sealed abstract class PokerHand(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card)
  extends Hand(Seq(card1, card2, card3, card4, card5)) {
  def rank(that: PokerHand): Option[PokerHand]
}

object PokerHand {
  def apply(cards: Seq[Card]): Option[PokerHand] = apply(cards(0), cards(1), cards(2), cards(3), cards(4))

  def apply(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card): Option[PokerHand] = {
    val cards = Seq(card1, card2, card3, card4, card5)

    StraightFlush(cards)
//    hand1 match {
//      case Some(a) => hand1
//      case None => {
//        val hand2 = FourOfAKind(cards)
//        hand2 match {
//          case Some(b) => hand2
//          case None => {
//            val hand3 = FullHouse(cards)
//            hand3 match {
//              case Some(c) => hand3
//              case None => {
//                val hand4 = Flush(cards)
//                hand4 match {
//                  case Some(d) => hand4
//                  case None => {
//                    val hand5 = Straight(cards)
//                    hand5 match {
//                      case Some(e) => hand5
//                      case None => HighCard(cards)
//                    }
//                  }
//                }
//              }
//            }
//          }
//        }
//      }
//    }
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
  def apply(cards: Seq[Card]): Option[PokerHand] = {
    isStraightFlush(cards) match {
      case true => Option(StraightFlush(cards.head, cards(1), cards(2), cards(3), cards(4)))
      case false => FourOfAKind(cards)
    }
  }

  def isStraightFlush(cards: Seq[Card]): Boolean = {
    val value = Cards(cards)
    value.sameSuits && value.isSequential
  }
}


case class FourOfAKind(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card)
  extends PokerHand(card1, card2, card3, card4, card5) {

  override def >(that: Hand): Boolean = {
    val A = this.cards.sortWith(_ > _)
    val B = that.cards.sortWith(_ > _)

    A.head > B.head
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[FourOfAKind]

  override def rank(that: PokerHand): Option[PokerHand] = {
    that match {
      case a: StraightFlush => return Option(that)
      case b: FourOfAKind => this == that match {
        case true => return None
        case false => that > this match {
          case true => return Option(that)
          case false => return Option(this)
        }
      }

      case _ => Option(this)
    }

    isFourOfAKind(that.cards) match {
      case true => {
        this > that match {
          case true => Option(this)
          case false => {
            that > this match {
              case true => Option(that)
              case false => None
            }
          }
        }
      }

      case false => {
        that match {
          case a: StraightFlush => Option(that)
          case _ => Option(this)
        }
      }
    }
  }

  def highestValuedCard: Option[Card] = {
    val groupedBy = cards.groupBy(c => c.value)
    groupedBy.filter(p => p._2.size == 4).last._2.headOption
  }
}

object FourOfAKind {
  def apply(cards: Seq[Card]): Option[PokerHand] = {
    isFourOfAKind(cards) match {
      case true => Option(FourOfAKind(cards.head, cards(1), cards(2), cards(3), cards(4)))
      case false => FullHouse(cards)
    }
  }

  def isFourOfAKind(cards: Seq[Card]): Boolean = {
    val groupByValue = cards.groupBy(c => c.value)
    groupByValue.size ==2 && groupByValue.exists(p => p._2.size == 4)
  }
}


case class FullHouse(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends PokerHand(card1, card2, card3, card4, card5) {
  private val groupedBy = cards.groupBy {c => c.value.value}

  private val cards2 = groupedBy.values.toSeq(1)

  override def rank(that: PokerHand): Option[PokerHand] = {
    isFullHouse(that.cards) match {
      case true => {
        val x = this
        val y: FullHouse = that.asInstanceOf[FullHouse]

        x.cards2(0) > y.cards2(0) match {
          case true => Option(that)
          case false => None
        }
      }

      case false => {
        that match {
          case a: FourOfAKind => Option(that)
          case _ => None
        }
      }
    }
  }
}

object FullHouse {
  def apply(cards: Seq[Card]): Option[PokerHand] = {
    isFullHouse(cards) match {
      case true => Option(FullHouse(cards(0), cards(1), cards(2), cards(3), cards(4)))
      case false => Flush(cards)
    }
  }

  def isFullHouse(cards: Seq[Card]): Boolean = {
    val groupedBy = cards.groupBy(c => c.value.value)

    groupedBy.size == 2 match {
      case true => {
        val values = groupedBy.values.toSeq.sortWith(_.size < _.size)

        val cards1: Seq[Card] = values(0)
        val cards2: Seq[Card] = values(1)

        cards1.size == 2 && cards2.size == 3
      }

      case false => false
    }
  }
}


case class Flush(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card)
  extends PokerHand(card1, card2, card3, card4, card5) {
  override def rank(that: PokerHand): Option[PokerHand] = {
    that match {
      case a: FullHouse => Option(a)
      case b: FourOfAKind => Option(b)
      case _ => None
    }
  }
}

object Flush {
  def apply(cards: Seq[Card]): Option[PokerHand] = {
    isFlush(cards) match {
      case true => Option(Flush(cards(0), cards(1), cards(2), cards(3), cards(4)))
      case false => Straight(cards)
    }
  }

  def isFlush(cards: Seq[Card]): Boolean = cards.groupBy(c => c.suit).size == 1
}


case class Straight(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends PokerHand(card1, card2, card3, card4, card5) {
  override def rank(that: PokerHand): Option[PokerHand] = ???
}

object Straight {
  def apply(cards: Seq[Card]): Option[PokerHand] = {
    isStraight(cards) match {
      case true => Option(Straight(cards(0), cards(1), cards(2), cards(3), cards(4)))
      case false => HighCard(cards)
    }
  }

  def isStraight(cards: Seq[Card]): Boolean = {
    val value = Cards(cards)
    value.isSequential && !value.sameSuits
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