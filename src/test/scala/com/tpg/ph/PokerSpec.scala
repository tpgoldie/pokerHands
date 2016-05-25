package com.tpg.ph

import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}

class PokerSpec extends FeatureSpec with Matchers with GivenWhenThen {
  feature("a player wins") {
    scenario("white player wins") {

      Given("input of Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C AH")
      val input = "Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C AH"

      When("Calculating output")
      val output = Poker(input).output

      Then("white wins")
      And("Output is: White wins. - with high card: Ace")
      output should be("White wins. - with high card: Ace")
    }
  }
}
