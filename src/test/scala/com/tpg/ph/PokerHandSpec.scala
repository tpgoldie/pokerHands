package com.tpg.ph

class PokerHandSpec extends HandSpec {
  describe("A poker hand") {
    describe("highest valued card") {
      val hand: Hand = PokerHand(Card(Nine, Hearts), Card(Two, Hearts), Card(Seven, Diamonds), Card(Four, Spades),
        Card(Three, Diamonds)).get

      it("should give the highest valued card for 0 index") {
        hand.highestValuedCard(0) should be(Option(Card(Nine, Hearts)))
      }

      it("should give the highest valued card for 1 index") {
        hand.highestValuedCard(1) should be(Option(Card(Seven, Diamonds)))
      }

      it("should give the highest valued card for 2 index") {
        hand.highestValuedCard(2) should be(Option(Card(Four, Spades)))
      }

      it("should give the highest valued card for 3 index") {
        hand.highestValuedCard(3) should be(Option(Card(Three, Diamonds)))
      }

      it("should give the highest valued card for 4 index") {
        hand.highestValuedCard(4) should be(Option(Card(Two, Hearts)))
      }

      it("should give nothing for out of range index 5") {
        hand.highestValuedCard(5) should be(None)
      }
    }

    describe("construction") {
      it("can construct a straight flush poker hand") {
        val hand = PokerHand(Card(Two, Hearts), Card(Three, Hearts), Card(Four, Hearts), Card(Five, Hearts), Card(Six, Hearts))

        hand map { h => h.isInstanceOf[StraightFlush] should be(true) }
      }

      it("can construct a high card poker hand") {
        val hand = PokerHand(Card(Two, Hearts), Card(Three, Diamonds), Card(Four, Hearts), Card(Five, Spades), Card(Six, Hearts))

        hand map { h => h.isInstanceOf[HighCard] should be(true) }
      }
    }
  }
}
