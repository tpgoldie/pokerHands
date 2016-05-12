package com.tpg.ph

case class HighestValuedCardMapping(cards: Seq[Card]) {
  private val mapping: Map[Int, HighestValuedCardAlgorithm] = {
    val pairs = 0 until cards.size map { i => (i, HighestValuedCardAlgorithm(cards.sortWith(_ > _).drop(i)))}
    pairs.toMap
  }

  def highestValuedCard(index: Int): HighestValuedCard = HighestValuedCard(index + 1, mapping(index).is)
}
