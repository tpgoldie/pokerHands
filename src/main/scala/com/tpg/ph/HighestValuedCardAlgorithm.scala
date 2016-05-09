package com.tpg.ph

case class HighestValuedCardAlgorithm(cards: Seq[Card]) {
  def is: Option[Card] = {
    val sortedCards = cards.sortWith(_ > _)

    val first = sortedCards.headOption
    val second = sortedCards.tail.headOption

    val values = Seq(first, second)
    values.flatten.sortWith(_ > _)

    values(0)
  }
}

