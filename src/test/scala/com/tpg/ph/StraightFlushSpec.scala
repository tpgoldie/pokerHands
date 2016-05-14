package com.tpg.ph

import org.scalatest.{FunSpec, Matchers}

class StraightFlushSpec extends FunSpec with Matchers {
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

    it("ranks a straight flush poker hand higher than a high card poker hand") {
      val A: Option[PokerHand] = StraightFlush(Seq(Two, Three, Four, Five, Six) map {value => Card(value, Hearts) })
      val B: Option[PokerHand] = HighCard(Seq(Four, Five, Ten, Seven, Eight) map {value => Card(value, Diamonds) })

      assertRanking(A, B, A)
    }

    it("ranks straight flushes with the same hand as undetermined") {
      val A: Option[PokerHand] = StraightFlush(Seq(Two, Three, Four, Five, Six) map {value => Card(value, Hearts) })

      assertRanking(A, A, None)
    }
  }

  private def assertRanking(handA: Option[PokerHand], handB: Option[PokerHand], expectedOutcome: Option[PokerHand]) = {
    val values = Seq(handA, handB).flatten
    values.head.rank(values.last) map { result => result should be(expectedOutcome.get) }
  }
}
