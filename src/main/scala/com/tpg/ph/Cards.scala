package com.tpg.ph

case class Cards(cards: Seq[Card]) {
  def sameSuits: Boolean = cards.groupBy(c => c.suit).size == 1

  def isSequential: Boolean = {
    val sorted = cards.sortWith(_ < _)
    val range = sorted.head.value to sorted.last.value
    range == sorted.map(c => c.value)
  }
}
