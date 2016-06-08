package com.tpg.ph

case class HighestPair(a: TwoPairs, b: TwoPairs) {
  lazy val value = a.highestPair(0) > b.highestPair(0) match {
    case true => Option(a)
    case false => {
      b.highestPair(0) > a.highestPair(0) match {
        case true => Option(b)
        case false => {
          a.highestPair(1) > b.highestPair(1) match {
            case true => Option(a)
            case false => {
              b.highestPair(1) > a.highestPair(1) match {
                case true => Option(b)
                case false => None
              }
            }
          }
        }
      }
    }
  }
}
