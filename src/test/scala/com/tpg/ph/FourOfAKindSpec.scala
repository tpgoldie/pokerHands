package com.tpg.ph

class FourOfAKindSpec extends HandSpec {
  describe("four of a kind") {
    it("should contain four cards of the same value") {
      val hand: Option[PokerHand] = FourOfAKind(Seq(Two, Two, Two, Two, Six) map { value => Card(value, Hearts) })
      hand.map(h => h.isInstanceOf[FourOfAKind] should be(true))
    }

    it("should not construct a four of a kind hand if it doesn't contain four cards of the same value") {
      val hand: Option[PokerHand] = FourOfAKind(Seq(Two, Three, Two, Two, Six) map { value => Card(value, Hearts) })
      hand.map(h => h.isInstanceOf[FourOfAKind] should be(false))
    }

    it("ranks by the value of the 4 cards") {
      val A: Option[PokerHand] = FourOfAKind(Seq(Two, Two, Two, Two, Six) map {value => Card(value, Hearts) })
      val B: Option[PokerHand] = FourOfAKind(Seq(Three, Three, Three, Three, Eight) map {value => Card(value, Diamonds) })

      assertRanking(A, B, B)
    }

    it("rank undefined if the value of the 4 cards are the same") {
      val A: Option[PokerHand] = FourOfAKind(Seq(Two, Two, Two, Two, Six) map {value => Card(value, Hearts) })

      assertRanking(A, A, None)
    }
  }
}
