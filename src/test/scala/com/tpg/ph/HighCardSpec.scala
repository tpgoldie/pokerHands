package com.tpg.ph

import com.tpg.ph.ranking.Rank
import org.scalatest.{FunSpec, Matchers}

class HighCardSpec extends FunSpec with Matchers {
  describe("A high card ranking") {
    describe("ranks hands which do not fit any higher category") {
      describe("by the value of their highest card") {
        it("should rank A > B") {
          val A: PokerHand = PokerHand(Card(Two, Hearts), Card(Four, Clubs), Card(Five, Spades), Card(Seven, Spades), Card(Eight, Diamonds)).get
          val B: PokerHand = PokerHand(Card(Three, Clubs), Card(Five, Clubs), Card(Six, Diamonds), Card(Six, Hearts), Card(Seven, Clubs)).get

          A.rank(B) map { result => result should be(A) }
        }
      }

      describe("if the two hands have the same highest card, the hands are ranked by the value of the next highest card") {
        it("should rank B > A") {
          val A: PokerHand = PokerHand(Card(Two, Hearts), Card(Four, Clubs), Card(Five, Spades), Card(Six, Spades), Card(Eight, Diamonds)).get
          val B: PokerHand = PokerHand(Card(Three, Clubs), Card(Five, Clubs), Card(Six, Diamonds), Card(Seven, Hearts), Card(Eight, Clubs)).get

          A.rank(B) map { result => result should be(B) }
        }
      }

      describe("if the two hands contain the same cards, the hands are considered equal") {
        it("should rank B > A") {
          val A: PokerHand = PokerHand(Card(Two, Hearts), Card(Four, Clubs), Card(Five, Spades), Card(Six, Spades), Card(Eight, Diamonds)).get

          A.rank(A) map { result => result should be(None) }
        }
      }
    }
  }
}
