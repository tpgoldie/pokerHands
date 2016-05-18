package com.tpg.ph

import org.scalatest.{FunSpec, Matchers}

abstract class HandSpec extends FunSpec with Matchers {
  protected def assertRanking(handA: Option[PokerHand], handB: Option[PokerHand], expectedOutcome: Option[PokerHand]) = {
    val values = Seq(handA, handB).flatten
    values.head.rank(values.last) map { result => result should be(expectedOutcome.get) }
  }
}
