package com.tpg.ph

case class Player(name: String, tokens: Seq[String]) {
  val hand: PokerHand = PokerHand(tokens map { token => Card(token) } flatten).get

  def rank(that: Player): Option[PokerHand] = this.hand.rank(that.hand)
}
