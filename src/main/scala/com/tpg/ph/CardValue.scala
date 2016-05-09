package com.tpg.ph

sealed abstract class CardValue(val value: Int) {
  def >(that: CardValue): Boolean = this.value > that.value

  override def toString: String = s"${value}"
}

object CardValue {
  def apply(value: Int): Option[CardValue] = {
    value match {
      case 1 => Some(One)
      case 2 => Some(Two)
      case 3 => Some(Three)
      case 4 => Some(Four)
      case 5 => Some(Five)
      case 6 => Some(Six)
      case 7 => Some(Seven)
      case 8 => Some(Eight)
      case 9 => Some(Nine)
      case 10 => Some(Ten)
      case 11 => Some(Jack)
      case 12 => Some(Queen)
      case 13 => Some(King)
      case _ => None
    }
  }
}

case object One extends CardValue(1)

case object Two extends CardValue(2)

case object Three extends CardValue(3)

case object Four extends CardValue(4)

case object Five extends CardValue(5)

case object Six extends CardValue(6)

case object Seven extends CardValue(7)

case object Eight extends CardValue(8)

case object Nine extends CardValue(9)

case object Ten extends CardValue(10)

case object Jack extends CardValue(11)

case object Queen extends CardValue(12)

case object King extends CardValue(13)