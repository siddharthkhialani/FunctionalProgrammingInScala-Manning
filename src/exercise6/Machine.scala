package exercise6

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def nextState(input: Input): Machine = {
    input match {
      case Coin if locked && candies != 0 => Machine(locked = false, candies, coins + 1)
      case Turn if !locked => Machine(locked = true, candies - 1, coins)
      case _ => Machine(locked, candies, coins)
    }
  }
}

