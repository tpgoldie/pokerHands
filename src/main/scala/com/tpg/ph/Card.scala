package com.tpg.ph

case class Card(val value: CardValue, val suit: Suit) {
  def >(that: Card): Boolean = this.value > that.value
  def <(that: Card): Boolean = this.value < that.value

  def canEqual(obj: Any): Boolean = obj.isInstanceOf[Card]

  override def equals(that: Any): Boolean = that match {
    case that: Card => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def hashCode: Int = {
    val prime = 31

    prime + value.value
//    prime * result + suit.hashCode
  }

  override def toString: String = s"${value} ${suit}"
}

object Card {
  def apply(value: String): Option[Card] = {
    val cv = CardValue(value(0))
    val suit = Suit(value(1))

    cv == None || suit == None match {
      case false => Option(Card(cv.get, suit.get))
      case true => None
    }
  }

  def apply(value: Int, suit: Suit): Option[Card] = {
    val cv: Option[CardValue] = CardValue(value)
    cv map { x => Card(x, suit) }
  }
}
