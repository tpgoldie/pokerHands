package com.tpg.ph.ranking

import com.tpg.ph.Hand

import scala.annotation.tailrec

sealed abstract class Rank(handA: Option[Hand], handB: Option[Hand]) {
  def rank: Option[Hand] = {
    val values = Seq(handA, handB).flatten
    values.size == 2 match {
      case true => calculateRank(handA.get, handB.get)

      case false => None
    }
  }

  protected def calculateRank(handA: Hand, handB: Hand): Option[Hand]
}

case class HighCard(handA: Option[Hand], handB: Option[Hand]) extends Rank(handA, handB) {
  override protected def calculateRank(handA: Hand, handB: Hand): Option[Hand] = {
    handA.cards.size == handB.cards.size match {
      case true => highestValueCard(handA, handB, 0)
      case false => None
    }
  }

  @tailrec
  private def highestValueCard(handA: Hand, handB: Hand, index: Int): Option[Hand] = {
    index == handA.cards.size match {
      case true => None
      case false => {
        handA.highestValuedCard(index) == handB.highestValuedCard(index) match {
          case true => highestValueCard(handA, handB, index+1)
          case false => Option(Seq(handA, handB).sortWith(_.highestValuedCard(index).get > _.highestValuedCard(index).get).head)
        }
      }
    }
  }
}

object HighCard {
  def apply(handA: Hand, handB: Hand): HighCard = new HighCard(Option(handA), Option(handB))
}
