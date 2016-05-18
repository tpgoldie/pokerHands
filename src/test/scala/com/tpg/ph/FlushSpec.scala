package com.tpg.ph

class FlushSpec extends HandSpec {
  describe("A flush") {
    val values = Seq(Two, Four, Six, Seven, Jack)
    val cards = values map { v => Card(v, Hearts) }

    it("contains 5 cards of the same suit") {
      val hand = PokerHand(cards(0), cards(1), cards(2), cards(3), cards(4))
      hand map { h => h.isInstanceOf[Flush] should be(true) }
    }

    it("ranks lower than a straight flush") {
      val A: Option[PokerHand] = Flush(cards)
      val range = Two to Six
      val B: Option[PokerHand] = StraightFlush(range map { cv => Card(cv, Hearts)})

      assertRanking(A, B, B)
    }

    it("ranks lower than a four of a kind") {
      val A: Option[PokerHand] = Flush(cards)
      val B: Option[PokerHand] = FourOfAKind(Seq(Hearts, Diamonds, Spades, Clubs).map(suit => Card(Two, suit))
        ++ Seq(Card(Three, Hearts)))

      assertRanking(A, B, B)
    }

    it("ranks lower than a full house") {
      val cards1 = Seq(Hearts, Diamonds, Spades) map { value => Card(Two, value) }
      val cards2 = Seq(Diamonds, Clubs) map { value => Card(Three, value) }

      val A: Option[PokerHand] = Flush(cards)
      val B: Option[PokerHand] = FullHouse(cards1 ++ cards2)

      assertRanking(A, B, B)
    }

    it("ranks same flushes as undefined") {
      val A: Option[PokerHand] = Flush(cards)

      assertRanking(A, A, None)
    }
  }
}
