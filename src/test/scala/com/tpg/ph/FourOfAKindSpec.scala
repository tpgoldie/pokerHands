package com.tpg.ph

class FourOfAKindSpec extends HandSpec {
  describe("four of a kind") {
    val suits = Seq(Diamonds, Spades, Hearts, Clubs)

    it("should contain four cards of the same value") {
      val hand: Option[PokerHand] = FourOfAKind(suits.map(value => Card(Two, value)) ++ Seq(Card(Three, Hearts)))
      hand.map(h => h.isInstanceOf[FourOfAKind] should be(true))
    }

    it("should not construct a four of a kind hand if it doesn't contain four cards of the same value") {
      val hand: Option[PokerHand] = FourOfAKind(suits.drop(1).map(value => Card(Two, value)) ++
        Seq(Card(Six, Diamonds), Card(Four, Diamonds)))
      hand.map(h => h.isInstanceOf[FourOfAKind] should be(false))
    }

    it("ranks by the value of the 4 cards") {
      val A: Option[PokerHand] = FourOfAKind(suits.map(value => Card(Two, value)) ++ Seq(Card(Four, Hearts)))
      val B: Option[PokerHand] = FourOfAKind(suits.map(value => Card(Three, value)) ++ Seq(Card(Seven, Diamonds)))

      assertRanking(A, B, B)
    }

    it("rank undefined if the value of the 4 cards are the same") {
      val A: Option[PokerHand] = FourOfAKind(suits.map(value => Card(Two, value)) ++ Seq(Card(Five, Diamonds)))

      assertRanking(A, A, None)
    }

    it("ranks lower than a straight flush") {
      val A: Option[PokerHand] = StraightFlush(Two to Six map { v => Card(v, Hearts)})
      val B: Option[PokerHand] = FourOfAKind(suits.map(s => Card(Two, s)) ++ Seq(Card(Three, Hearts)))

      assertRanking(B, A, A)
    }

    it("ranks higher than a three of a kind") {
      val cards1 = Two to Four map { v => Card(v, Hearts)}
      val cards2 = Seq(Eight, Nine) map { v => Card(v, Diamonds) }

      val A: Option[PokerHand] = FullHouse(cards1 ++ cards2)
      val B: Option[PokerHand] = FourOfAKind(suits.map(s => Card(Two, s)) ++ Seq(Card(Three, Hearts)))

      assertRanking(B, A, B)
    }
  }
}
