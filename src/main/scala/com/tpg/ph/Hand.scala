package com.tpg.ph

import scala.util.Try

abstract class Hand(val cards: Seq[Card]) {
  def >[T <: Hand](that: T): Boolean = {
    val v1 = this.cards.sortWith(_ > _)(0)
    val v2 = that.cards.sortWith(_ > _)(0)

    v1 > v2
  }

  def highestValuedCard(index: Int): Option[Card] = Try(Option(cards.sortWith(_ > _)(index))).getOrElse(None)

  def canEqual(obj: Any): Boolean = obj.isInstanceOf[Hand]

  override def equals(that: Any): Boolean = that match {
    case that: Hand => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def hashCode: Int = 31 + cards.hashCode

  override def toString: String = s"${cards.mkString(", ")}"
}


