package com.tpg.ph

import org.scalatest.{FunSpec, Matchers}

class HighestValuedCardMappingSpec extends FunSpec with Matchers {
  val mapping = HighestValuedCardMapping(Seq(Card(Ten, Hearts), Card(Five, Diamonds), Card(Jack, Spades)))

  describe("A highest valued card mapping") {
    describe("gives the highest valued card for index 0") {
      mapping.highestValuedCard(0) should be(HighestValuedCard(1, Option(Card(Jack, Spades))))
    }

    describe("gives the next highest valued card for index 1") {
      mapping.highestValuedCard(1) should be(HighestValuedCard(2, Option(Card(Ten, Hearts))))
    }

    describe("gives the next highest valued card for index 2") {
      mapping.highestValuedCard(1) should be(HighestValuedCard(2, Option(Card(Ten, Hearts))))
    }
  }
}
