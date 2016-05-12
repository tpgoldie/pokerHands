package com.tpg.ph.ranking

import com.tpg.ph.{Diamonds, Six, Three, _}
import org.scalatest.{FunSpec, Matchers}

class HighCardSpec extends FunSpec with Matchers {
  describe("A high card ranking") {
    describe("ranks hands which do not fit any higher category") {
      describe("by the value of their highest card") {
        it("should rank A > B") {
          val A: Hand = PokerHand(Card(Two, Hearts), Card(Four, Clubs), Card(Five, Spades), Card(Seven, Spades), Card(Eight, Diamonds)).get
          val B: Hand = PokerHand(Card(Three, Clubs), Card(Five, Clubs), Card(Six, Diamonds), Card(Six, Hearts), Card(Seven, Clubs)).get

          val ranking: Rank = HighCard(A, B)
          ranking.rank should be(Option(A))
        }
      }

      describe("if the two hands have the same highest card, the hands are ranked by the value of the next highest card") {
        it("should rank B > A") {
          val A: Hand = PokerHand(Card(Two, Hearts), Card(Four, Clubs), Card(Five, Spades), Card(Six, Spades), Card(Eight, Diamonds)).get
          val B: Hand = PokerHand(Card(Three, Clubs), Card(Five, Clubs), Card(Six, Diamonds), Card(Seven, Hearts), Card(Eight, Clubs)).get

          val ranking: Rank = HighCard(A, B)
          ranking.rank should be(Option(B))
        }
      }

      describe("if the two hands contain the same cards, the hands are considered equal") {
        it("should rank B > A") {
          val A: Hand = PokerHand(Card(Two, Hearts), Card(Four, Clubs), Card(Five, Spades), Card(Six, Spades), Card(Eight, Diamonds)).get

          val ranking: Rank = HighCard(A, A)
          ranking.rank should be(None)
        }
      }
    }
  }
}
