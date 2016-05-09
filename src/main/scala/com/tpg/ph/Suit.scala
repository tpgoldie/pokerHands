package com.tpg.ph

sealed trait Suit {
}

case object Hearts extends Suit {
  override def toString: String = "Hearts"
}

case object Clubs extends Suit {
  override def toString: String = "Clubs"
}

case object Diamonds extends Suit {
  override def toString: String = "Diamonds"
}

case object Spades extends Suit {
  override def toString: String = "Spades"
}


