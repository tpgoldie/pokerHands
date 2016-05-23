package com.tpg.ph

class StraightSpec extends HandSpec {
  describe("a straight") {
    val cards = Seq(Card(Two, Hearts), Card(Three, Diamonds), Card(Four, Clubs), Card(Five, Diamonds), Card(Six, Clubs))

    it("contains 5 cards with consecutive values") {
      val hand = PokerHand(cards)
      hand map { h => h.isInstanceOf[Straight] should be(true) }
    }

    it("ranks lower than a straight flush") {
      val A: Option[PokerHand] = Straight(cards)
      val B: Option[PokerHand] = StraightFlush(Seq(Four, Five, Six, Seven, Eight) map {value => Card(value, Diamonds) })

      assertRanking(A, B, B)
    }

    it("ranks lower than a four of a kind") {
      val A: Option[PokerHand] = Straight(cards)
      val B: Option[PokerHand] = FourOfAKind(Seq(Hearts, Clubs, Diamonds, Spades).map { suit => Card(Four, suit) } ++ Seq(Card(Ten, Clubs)))

      assertRanking(A, B, B)
    }

    it("ranks lower than a full house") {
      val A: Option[PokerHand] = Straight(cards)

      val cards1 = Seq(Hearts, Diamonds, Spades) map { value => Card(Two, value) }
      val cards2 = Seq(Diamonds, Clubs) map { value => Card(Three, value) }

      val B: Option[PokerHand] = FullHouse(cards1 ++ cards2)

      assertRanking(A, B, B)
    }

    it("ranks lower than a flush") {
      val A: Option[PokerHand] = Straight(cards)

      val B: Option[PokerHand] = Flush(Seq(Two, Three, Four, Seven, Nine) map { value => Card(value, Hearts)})

      assertRanking(A, B, B)
    }

    it("ranks with another straight by the highest hand") {
      val A: Option[PokerHand] = Straight(cards)

      val cards2 = Seq(Card(Three, Diamonds), Card(Four, Clubs), Card(Five, Diamonds), Card(Six, Clubs), Card(Seven, Clubs))

      val B: Option[PokerHand] = Straight(cards2)

      assertRanking(A, B, B)
    }

    it("ranking with another equal straight hand") {
      val A: Option[PokerHand] = Straight(cards)

      assertUndefined(A, A)
    }
  }
}
