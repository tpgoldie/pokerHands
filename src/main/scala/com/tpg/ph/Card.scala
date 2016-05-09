package com.tpg.ph

case class Card(val value: CardValue, val suit: Suit) {
  def >(that: Card): Boolean = this.value > that.value

  override def toString: String = s"${value} ${suit}"
}

object Card {
  def apply(value: Int, suit: Suit): Option[Card] = {
    val cv: Option[CardValue] = CardValue(value)
    cv map { x => Card(x, suit) }
  }
}
