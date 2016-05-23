package com.tpg.ph

sealed abstract class Suit(val index: Int, val name: String) {
  override def toString: String = name
}

object Suit {
  def apply(value: Int): Option[Suit] = value match {
    case Hearts.index => Option(Hearts)
    case Clubs.index => Option(Clubs)
    case Diamonds.index => Option(Diamonds)
    case Spades.index => Option(Spades)
    case _ => None
  }
}

case object Hearts extends Suit(1, "Hearts")

case object Clubs extends Suit(2, "Clubs")

case object Diamonds extends Suit(3, "Diamonds")

case object Spades extends Suit(4, "Spades")



