package com.tpg.ph

class StraightFlushSpec extends HandSpec {
  describe("A straight flush") {
    it("should contain 5 cards of the same suit with consecutive values.") {
      val hand: Option[PokerHand] = StraightFlush(Seq(Two, Three, Four, Five, Six) map { value => Card(value, Hearts) })
      hand.map(h => h.isInstanceOf[StraightFlush])
    }

    it("ranks by the highest valued card in the hand") {
      val A: Option[PokerHand] = StraightFlush(Seq(Two, Three, Four, Five, Six) map {value => Card(value, Hearts) })
      val B: Option[PokerHand] = StraightFlush(Seq(Four, Five, Six, Seven, Eight) map {value => Card(value, Diamonds) })

      assertRanking(A, B, B)
    }

    it("ranks higher than a four of a kind") {
      val A: Option[PokerHand] = StraightFlush(Seq(Two, Three, Four, Five, Six) map {value => Card(value, Hearts) })
      val B: Option[PokerHand] = FourOfAKind(Seq(Hearts, Clubs, Diamonds, Spades).map(suit => Card(Four, suit))
        ++ Seq(Card(Seven, Diamonds)))

      assertRanking(A, B, A)
    }

    it("ranks higher than a full house") {
      val A: Option[PokerHand] = StraightFlush(Seq(Two, Three, Four, Five, Six) map {value => Card(value, Hearts) })
      val B: Option[PokerHand] = FullHouse(Seq(Hearts, Clubs, Diamonds, Spades).map(suit => Card(Four, suit))
        ++ Seq(Card(Seven, Diamonds)))

      assertRanking(A, B, A)
    }

    it("ranks higher than a high card hand") {
      val A: Option[PokerHand] = StraightFlush(Seq(Two, Three, Four, Five, Six) map {value => Card(value, Hearts) })
      val B: Option[PokerHand] = HighCard(Seq(Four, Five, Ten, Seven, Eight) map {value => Card(value, Diamonds) })

      assertRanking(A, B, A)
    }

    it("ranks straight flushes with the same hand as undetermined") {
      val A: Option[PokerHand] = StraightFlush(Seq(Two, Three, Four, Five, Six) map {value => Card(value, Hearts) })

      assertUndefined(A, A)
    }
  }
}
