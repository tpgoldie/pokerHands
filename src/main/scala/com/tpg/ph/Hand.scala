package com.tpg.ph

case class Hand(c1: Card, c2: Card, c3: Card, c4: Card, c5: Card) {
  private val cards: Seq[Card] = Seq(c1, c2, c3, c4, c5)

  private val highestValuedCardMapping: Map[Int, HighestValuedCardAlgorithm] = {
    val pairs = 0 to 4 map { i => (i, HighestValuedCardAlgorithm(cards.drop(i)))}
    pairs.toMap
  }

  def highestValuedCard(index: Int): HighestValuedCard = HighestValuedCard(index + 1, highestValuedCardMapping(index).is)

  override def toString: String = s"${cards.mkString(", ")}"
}

case class HighestValuedCard(index: Int, card: Option[Card]) {
  def >(that: HighestValuedCard): Boolean = {
    val values = Seq(this.card, that.card).flatten

    values.size match {
      case 2 => values(0) > values(1)
      case _ => false
    }
  }
}
