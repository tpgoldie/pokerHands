package com.tpg.ph

class ThreeOfAKindSpec extends HandSpec {
  describe("three of a kind") {
    val cards = Seq(Clubs, Diamonds, Spades).map { suit => Card(Four, suit)} ++ Seq(Card(Seven, Diamonds), Card(Nine, Hearts))

    it("contains three cards of the same value") {
      val hand = PokerHand(cards)
      hand.isInstanceOf[Option[ThreeOfAKind]] should be(true)
    }

    it("ranks lower than a straight flush") {
      val A: Option[PokerHand] = ThreeOfAKind(cards)
      val B: Option[PokerHand] = StraightFlush((Two to Six) map {value => Card(value, Hearts) })

      assertRanking(A, B, B)
    }

    it("ranks lower than a four of a kind") {
      val A: Option[PokerHand] = ThreeOfAKind(cards)
      val B: Option[PokerHand] = FourOfAKind(Seq(Diamonds, Spades, Hearts, Clubs).map(value => Card(Two, value)) ++
        Seq(Card(Four, Hearts)))

      assertRanking(A, B, B)
    }

    it("ranks lower than a full house") {
      val A: Option[PokerHand] = ThreeOfAKind(cards)

      val cards1 = Seq(Hearts, Diamonds, Spades) map { value => Card(Two, value) }
      val cards2 = Seq(Diamonds, Clubs) map { value => Card(Three, value) }

      val B: Option[PokerHand] = FullHouse(cards1 ++ cards2)

      assertRanking(A, B, B)
    }

    it("ranks lower than a flush") {
      val A: Option[PokerHand] = ThreeOfAKind(cards)

      val B: Option[PokerHand] = Flush(Seq(Two, Four, Six, Seven, Jack) map { v => Card(v, Hearts) })

      assertRanking(A, B, B)
    }

    it("ranks lower than a straight") {
      val A: Option[PokerHand] = ThreeOfAKind(cards)

      val B: Option[PokerHand] = Straight(Seq(Card(Two, Hearts), Card(Three, Diamonds), Card(Four, Clubs),
        Card(Five, Diamonds), Card(Six, Clubs))
      )

      assertRanking(A, B, B)
    }

    it("ranks by value of the 3 cards against a another three of a kind") {
      val A: Option[PokerHand] = ThreeOfAKind(cards)

      val cards2 = Seq(Clubs, Diamonds, Spades).map { suit => Card(Six, suit)} ++ Seq(Card(Seven, Diamonds), Card(Nine, Hearts))
      val B: Option[PokerHand] = ThreeOfAKind(cards2)

      assertRanking(A, B, B)
    }

    it("ranks an equal valued three of a kind as undefined") {
      val A = ThreeOfAKind(cards)

      assertUndefined(A, A)
    }

    it("ranks higher than two pairs") {
      val A = ThreeOfAKind(cards)

      val cards2 = Seq(Card(Two, Diamonds), Card(Two, Clubs), Card(Three, Hearts), Card(Three, Diamonds), Card(Nine, Spades))
      val B = TwoPairs(cards2)

      assertRanking(A, B, A)
    }
  }
}
