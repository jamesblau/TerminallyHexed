package terminallyhexed

import Assets._
import Entities._

object `package` {
  type IJ = (Int, Int) // output character (row, column) or offset of same
  type RC = (Int, Int) // hexagon (row, column); implicit w/Hex?
  type Rooms = Set[Int] // multiple for doors
  type Edge = Set[RC] // should only be two -- implicit to enforce?
}

object TerminallyHexed extends App {
  val stdIn = scala.io.Source.stdin

  val playerStarts = Map("A" -> GemMage, "C" -> PsyRat)

  val demoScenario = Demo
    .addPlayers(playerStarts)
    .setActive(Some(GemMage))

  val padTop = "\n" * 1
  val padBottom = "\n" * 1
  val padLeft = " " * 1

  def loop(scenario: Scenario): Unit = {
    println(padTop)
    scenario.drawn().map(padLeft + _) foreach println
    println(padBottom)
    val moveChars = "uiojkl"

    def move = (_: Char) match {
      case 'l' => scenario.move(x = 1)
      case 'u' => scenario.move(x = -1)
      case 'i' => scenario.move(y = 1)
      case 'k' => scenario.move(y = -1)
      case 'j' => scenario.move(z = 1)
      case 'o' => scenario.move(z = -1)
      case c => other(c)
    }

    def other = (_: Char) match {
      case 'q' => scenario.win
      case _ => scenario
    }

    lazy val next = stdIn.next() match {
      case y if moveChars.contains(y) => move(y)
      case c => other(c)
    }

    if (next.won)
      println("You won!")
    // else if (scenario.events.nonEmpty)
        // scenario.tick
    else
      loop(scenario = next)
  }
  loop(demoScenario)
}
