package com.tpg.ph

import org.scalatest.{FunSpec, Matchers}

class StraightFlushSpec extends FunSpec with Matchers {
  describe("A straight flush") {
    it("should contain 5 cards of the same suit with consecutive values.") {
      val hand: Option[PokerHand] = StraightFlush(Seq(Two, Three, Four, Five, Six) map {value => Card(value, Hearts) })
      hand.map(h => h.isInstanceOf[StraightFlush])
    }
  }
}
