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
//    val highestValueCard => Option[Hand] = (hand1: Hand, hand2: Hand, index: Int) => {
//      index == 6 match {
//        case true => None
//        case false => {
//          handA.highestValuedCard(index) == handB.highestValuedCard(index) match {
//            case true => highestValueCard(handA, handB, index+1)
//            case false => {
//              val mapping = Map(
//                handA.highestValuedCard(index).index -> handA,
//                handB.highestValuedCard(index).index -> handB
//              )
//
//              val hvc = mapping.values.map(v => v.highestValuedCard(index)).toSeq.sortWith(_ > _)(0)
//              Option(mapping(hvc.index))
//            }
//          }
//        }
//      }
//    }

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
          case false => Option(Seq(handA, handB).sortWith(_.highestValuedCard(index).get > _.highestValuedCard(index).get)(0))
        }
      }
    }
  }
}

object HighCard {
  def apply(handA: Hand, handB: Hand): HighCard = new HighCard(Option(handA), Option(handB))
}
