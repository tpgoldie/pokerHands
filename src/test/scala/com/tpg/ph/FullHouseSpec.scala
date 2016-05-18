package com.tpg.ph

class FullHouseSpec extends HandSpec {
  describe("full house") {
    val cards1 = Seq(Hearts, Diamonds, Spades) map { value => Card(Two, value) }
    val cards2 = Seq(Diamonds, Clubs) map { value => Card(Three, value) }

    it("should contain 3 cards of the same value, with the remaining 2 cards forming a pair") {
      val hand: Option[PokerHand] = FullHouse(cards1 ++ cards2)
      hand.map(h => h.isInstanceOf[FullHouse] should be(true))
    }

    it("ranks by the value of the 3 cards") {
      val cards3 = Seq(Hearts, Diamonds, Spades) map { value => Card(Five, value) }
      val cards4 = Seq(Diamonds, Clubs) map { value => Card(Four, value) }

      val A: Option[PokerHand] = FullHouse(cards1 ++ cards2)
      val B: Option[PokerHand] = FullHouse(cards3 ++ cards4)

      assertRanking(A, B, B)
    }

    it("ranks lower than a straight flush") {
      val cards3 = Seq(Hearts, Diamonds, Spades) map { value => Card(Five, value) }
      val cards4 = Seq(Diamonds, Clubs) map { value => Card(Four, value) }

      val A: Option[PokerHand] = StraightFlush(Two to Six map { v => Card(v, Hearts)})
      val B: Option[PokerHand] = FullHouse(cards3 ++ cards4)

      assertRanking(A, B, A)
      assertRanking(B, A, A)
    }

    it("ranks lower than a four of a kind") {
      val cards3 = Seq(Hearts, Diamonds, Spades) map { value => Card(Five, value) }
      val cards4 = Seq(Diamonds, Clubs) map { value => Card(Four, value) }

      val A: Option[PokerHand] = FourOfAKind(Seq(Hearts, Diamonds, Spades, Clubs).map(suit => Card(Two, suit)) ++ Seq(Card(Three, Diamonds)))
      val B: Option[PokerHand] = FullHouse(cards3 ++ cards4)

      assertRanking(A, B, A)
      assertRanking(B, A, A)
    }
  }
}
