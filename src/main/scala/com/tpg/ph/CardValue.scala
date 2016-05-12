package com.tpg.ph

import com.tpg.ph.CardValue.selectName

sealed abstract class CardValue(val name: String, val value: Int) {
  def >(that: CardValue): Boolean = this.value > that.value
  def <(that: CardValue): Boolean = this.value < that.value

  def to(that: CardValue): Seq[CardValue] = (this.value to that.value) map(i => CardValue(i).get)
  def until(that: CardValue): Seq[CardValue] = (this.value until that.value) map(i => CardValue(i).get)

  override def toString: String = s"${selectName(this.value)}"
}

object CardValue {
  def selectName(value: Int): String = value match {
    case 2 => Two.name
    case 3 => Three.name
    case 4 => Four.name
    case 5 => Five.name
    case 6 => Six.name
    case 7 => Seven.name
    case 8 => Eight.name
    case 9 => Nine.name
    case 10 => Ten.name
    case 11 => Jack.name
    case 12 => Queen.name
    case 13 => King.name
    case 14 => Ace.name

    case anythingElse => "n/a"
  }

  def apply(value: Int): Option[CardValue] = {
    value match {
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
      case 14 => Some(Ace)
      case _ => None
    }
  }
}

case object Two extends CardValue("Two", 2)

case object Three extends CardValue("Three", 3)

case object Four extends CardValue("Four", 4)

case object Five extends CardValue("Five", 5)

case object Six extends CardValue("Six", 6)

case object Seven extends CardValue("Seven", 7)

case object Eight extends CardValue("Eight", 8)

case object Nine extends CardValue("Nine", 9)

case object Ten extends CardValue("Ten", 10)

case object Jack extends CardValue("Jack", 11)

case object Queen extends CardValue("Queen", 12)

case object King extends CardValue("King", 13)

case object Ace extends CardValue("Ace", 14)