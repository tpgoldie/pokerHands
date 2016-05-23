package com.tpg.ph

class FlushSpec extends HandSpec {
  describe("A flush") {
    val values = Seq(Two, Four, Six, Seven, Jack)
    val cards = values map { v => Card(v, Hearts) }

    it("contains 5 cards of the same suit") {
      val hand = PokerHand(cards)
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

    it("ranks another flush hand value of first highest card") {
      val values = Seq(Two, Four, Six, Seven, Queen)
      val cards2 = values map { v => Card(v, Diamonds) }

      val A: Option[PokerHand] = Flush(cards)
      val B: Option[PokerHand] = Flush(cards2)

      assertRanking(A, B, B)
    }

    it("ranks another flush hand value of next highest card if first highest cards match") {
      val cards1 = Seq(Two, Three, Four, Six, Jack) map { v => Card(v, Hearts) }
      val cards2 = Seq(Two, Three, Five, Six, Jack) map { v => Card(v, Diamonds) }

      val A: Option[PokerHand] = Flush(cards1)
      val B: Option[PokerHand] = Flush(cards2)

      assertRanking(A, B, B)
    }

    it("ranking an equal flush hand is undefined") {
      val A: Option[PokerHand] = Flush(cards)

      assertUndefined(A, A)
    }

    it("ranks higher than a straight") {
      val A: Option[PokerHand] = Flush(cards)
      val B: Option[PokerHand] = Straight(Seq(Card(Two, Hearts), Card(Three, Diamonds), Card(Four, Clubs),
        Card(Five, Diamonds), Card(Six, Clubs)))

      assertRanking(A, B, A)
    }
  }
}
