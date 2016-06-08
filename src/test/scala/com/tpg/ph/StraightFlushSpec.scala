package com.tpg.ph

import scala.util.Random

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

    it("ranks higher than a flush") {
      val A: Option[PokerHand] = StraightFlush(Seq(Two, Three, Four, Five, Six) map {value => Card(value, Hearts) })
      val B: Option[PokerHand] = Flush(Seq(Two, Four, Six, Seven, Jack).map(value => Card(value, Hearts)))

      assertRanking(A, B, A)
    }

    it("ranks higher than a straight") {
      val A: Option[PokerHand] = StraightFlush(Seq(Two, Three, Four, Five, Six) map { v => Card(v, Hearts) })
      val B: Option[PokerHand] = Flush((Two to Six) map(v => Card(v, selectSuitRandomly.get)))

      assertRanking(A, B, A)
    }

    it("ranks higher than a three of a kind") {
      val A: Option[PokerHand] = StraightFlush(Seq(Two, Three, Four, Five, Six) map { v => Card(v, Hearts) })

      val cards = Seq(Clubs, Diamonds, Spades).map { suit => Card(Four, suit)} ++ Seq(Card(Seven, Diamonds), Card(Nine, Hearts))

      val B: Option[PokerHand] = ThreeOfAKind(cards)

      assertRanking(A, B, A)
    }

    it("ranks higher than a high card hand") {
      val A: Option[PokerHand] = StraightFlush(Seq(Two, Three, Four, Five, Six) map {value => Card(value, Hearts) })
      val B: Option[PokerHand] = HighCard(Seq(Four, Five, Ten, Seven, Eight) map { value => Card(value, Diamonds) })

      assertRanking(A, B, A)
    }

    it("ranks straight flushes with the same hand as undetermined") {
      val A: Option[PokerHand] = StraightFlush(Seq(Two, Three, Four, Five, Six) map { value => Card(value, Hearts) })

      assertUndefined(A, A)
    }
  }

  private def selectSuitRandomly: Option[Suit] = Suit(Random.nextInt(4) + 1)
}
