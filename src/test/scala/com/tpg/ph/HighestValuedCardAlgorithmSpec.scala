package com.tpg.ph

import org.scalatest.{FunSpec, Matchers}

class HighestValuedCardAlgorithmSpec extends FunSpec with Matchers {
  describe("Highest valued card algorithm") {
    it("should indicate the highest valued card in a sequence of different cards") {
      val actual = HighestValuedCardAlgorithm(Seq(Card(Two, Hearts), Card(Seven, Spades), Card(Five, Diamonds)))
      actual.is should be(Option(Card(Seven, Spades)))
    }
  }
}
