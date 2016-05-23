package com.tpg.ph

class ThreeOfAKindSpec extends HandSpec {
  describe("three of a kind") {
    val cards = Seq(Clubs, Diamonds, Spades).map { suit => Card(Four, suit)} ++ Seq(Card(Seven, Diamonds), Card(Nine, Hearts))

    it("contains three cards of the same value") {
      val hand = PokerHand(cards)
      hand.isInstanceOf[Option[ThreeOfAKind]] should be(true)
    }
  }
}
