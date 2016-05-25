package com.tpg.ph

import com.tpg.ph.Poker.Tie


case class Poker(input: String) {
  private val tokens = input.trim.split(" ")

  private def toTokens(indices: Seq[Int]): Seq[String] = indices map { i => tokens(i) }

  private val playerOne = Player(tokens(0).dropRight(1), toTokens(Seq(1, 2, 3, 4, 5)))

  private val playerTwo = Player(tokens(7).dropRight(1), toTokens(Seq(8, 9, 10, 11, 12)))

  val output: String = {
    playerOne.rank(playerTwo) match {
      case Some(r) => {
        r == playerOne.hand match {
          case true => wins(playerOne)
          case false => {
            r == playerTwo.hand match {
              case true => wins(playerTwo)
              case false => Tie
            }
          }
        }
      }

      case None => Tie
    }
  }

  private def wins(player: Player) = {
    val output = player.hand.isInstanceOf[HighCard] match {
      case true => s"high card: ${player.hand.highestValuedCard(0).get.value}"
      case false => ""
    }

    s"${player.name} wins. - with $output"
  }
}


object Poker {
  val Tie = "Tie"
}