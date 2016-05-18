package com.tpg.ph

class StraightSpec extends HandSpec {
  describe("a straight") {
    it("contains 5 cards with consecutive values") {
      val cards = Seq(Card(Two, Hearts), Card(Three, Diamonds), Card(Four, Clubs), Card(Five, Diamonds), Card(Six, Clubs))
      val hand = PokerHand(cards)
      hand map { h => h.isInstanceOf[Straight] should be(true) }
    }
  }
}
