package com.tpg.ph

class TwoPairsSpec extends HandSpec {
  describe("Two pairs") {
    val cards = Seq(Card(Two, Diamonds), Card(Two, Clubs), Card(Three, Hearts), Card(Three, Diamonds), Card(Nine, Spades))

    it("contains two different pairs") {
      PokerHand(cards).map { h => h.isInstanceOf[TwoPairs] should be(true) }
    }

    it("ranks lower than a straight flush") {
      val A = TwoPairs(cards)

      val B = StraightFlush(Seq(Two, Three, Four, Five, Six) map { v => Card(v, Hearts) })

      assertRanking(A, B, B)
    }

    it("ranks lower than a four of a kind") {
      val A = TwoPairs(cards)

      val suits = Seq(Diamonds, Spades, Hearts, Clubs)

      val B: Option[PokerHand] = FourOfAKind(suits.map(s => Card(Two, s)) ++ Seq(Card(Seven, Hearts)))

      assertRanking(A, B, B)
    }

    it("ranks lower than a full house") {
      val A = TwoPairs(cards)

      val cards1 = Seq(Hearts, Diamonds, Spades) map { value => Card(Two, value) }
      val cards2 = Seq(Diamonds, Clubs) map { value => Card(Three, value) }

      val B: Option[PokerHand] = FullHouse(cards1 ++ cards2)

      assertRanking(A, B, B)
    }

    it("ranks lower than a flush") {
      val A = TwoPairs(cards)

      val cards2 = Seq(Two, Four, Six, Seven, Jack) map { v => Card(v, Hearts) }

      val B: Option[PokerHand] = Flush(cards2)

      assertRanking(A, B, B)
    }

    it("ranks lower than a straight") {
      val A = TwoPairs(cards)

      val cards2 = Seq(Card(Two, Hearts), Card(Three, Diamonds), Card(Four, Clubs), Card(Five, Diamonds), Card(Six, Clubs))

      val B: Option[PokerHand] = Straight(cards2)

      assertRanking(A, B, B)
    }

    it("ranks lower than a three of a kind") {
      val A = TwoPairs(cards)

      val cards2 = Seq(Clubs, Diamonds, Spades).map { suit => Card(Four, suit)} ++ Seq(Card(Seven, Diamonds), Card(Nine, Hearts))
      val B = ThreeOfAKind(cards2)

      assertRanking(A, B, B)
    }

    it("ranks an another two pairs by highest value of the highest pair") {
      val A = TwoPairs(cards)

      val cards2 = Seq(Card(Two, Diamonds), Card(Two, Clubs), Card(Four, Hearts), Card(Four, Diamonds), Card(Nine, Spades))
      val B = PokerHand(cards2)

      assertRanking(A, B, B)
    }

    it("ranks higher than a high card") {
      val A = TwoPairs(cards)
      val B = PokerHand(Card(Three, Clubs), Card(Five, Clubs), Card(Six, Diamonds), Card(Six, Hearts), Card(Seven, Clubs))

      assertRanking(A, B, A)
    }

    it("ranks an equal valued two pairs as undefined") {
      val A = TwoPairs(cards)

      assertUndefined(A, A)
    }
  }
}
