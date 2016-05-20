package com.tpg.ph

import org.scalatest.{FunSpec, Matchers}

abstract class HandSpec extends FunSpec with Matchers {
  protected def assertRanking(handA: Option[PokerHand], handB: Option[PokerHand], expectedOutcome: Option[PokerHand]) = {
    val result = rank(handA, handB)

    result should not be None
    result map { outcome => outcome should be(expectedOutcome.get) }
  }

  protected def rank(handA: Option[PokerHand], handB: Option[PokerHand]): Option[PokerHand] = {
    val values = Seq(handA, handB).flatten
    values.head.rank(values.last)
  }

  protected def assertUndefined(handA: Option[PokerHand], handB: Option[PokerHand]) = rank(handA, handB) should be (None)
}
