package com.tpg.ph

case class Ranking(handA: Hand, handB: Hand) {
  def rank: Rank = {
    val aHVC = handA.highestValuedCard(0)
    val bHVC = handB.highestValuedCard(0)

    aHVC > bHVC match {
      case true => HighCard(handA)
      case false => aHVC == bHVC match {
        case true => HighCard(None)
        case false => HighCard(handB)
      }
    }
  }
}
