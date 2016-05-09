package com.tpg.ph

import org.scalatest.{FunSpec, Matchers}

class HighestValuedCardSpec extends FunSpec with Matchers {
  describe("A highest valued card") {
    describe("greater than") {
      it("should be true when comparing a higher highest valued card against another") {
        val hvc1 = HighestValuedCard(1, Some(Card(Seven, Hearts)))
        val hvc2 = HighestValuedCard(1, Some(Card(Eight, Diamonds)))
        hvc2 > hvc1 should be(true)
      }

      it("should be false when comparing two highest valued cards of the same value") {
        val hvc1 = HighestValuedCard(1, Some(Card(Seven, Hearts)))
        val hvc2 = HighestValuedCard(1, Some(Card(Seven, Diamonds)))
        hvc2 > hvc1 should be(false)
      }

      it("should be false when comparing a lower highest valued cards against another") {
        val hvc1 = HighestValuedCard(1, Some(Card(Seven, Hearts)))
        val hvc2 = HighestValuedCard(1, Some(Card(Eight, Diamonds)))
        hvc1 > hvc2 should be(false)
      }
    }
  }
}
