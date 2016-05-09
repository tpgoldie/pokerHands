package com.tpg.ph

sealed abstract class Rank(hand: Option[Hand]) {
}

case class HighCard(hand: Option[Hand]) extends Rank(hand) {
  override def toString: String = {
    hand match {
      case Some(x) => x.toString
      case None => ""
    }
  }
}

object HighCard {
  def apply(hand: Hand): HighCard = HighCard(Some(hand))
}