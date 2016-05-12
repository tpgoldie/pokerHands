package com.tpg.ph

case class HighestValuedCard(index: Int, card: Option[Card]) {
  def >(that: HighestValuedCard): Boolean = {
    val values = Seq(this.card, that.card).flatten

    values.size match {
      case 2 => values(0) > values(1)
      case _ => false
    }
  }

  def canEqual(obj: Any): Boolean = obj.isInstanceOf[HighestValuedCard]

  override def equals(that: Any): Boolean = that match {
    case that: HighestValuedCard => {
      val a = this.hashCode
      val b = that.hashCode

      that.canEqual(this) && (a == b)
    }

    case _ => false
  }

  override def hashCode: Int = {
    val prime = 31
    var result = 1

    result = prime * result + this.index
    prime * result + card.map(c => c.value.value).getOrElse(index)
  }
}
