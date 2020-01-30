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

  // val playerStarts = Map("A" -> GemMage, "C" -> PsyRat)
  val playerStarts = Map("A" -> GemMage)

  val demoScenario = Demo.addPlayers(playerStarts).copy(trapDamage = 3)

  val padTop = "\n" * 1
  val padBottom = "\n" * 1
  val padLeft = " " * 1

  def loop(scenario: Scenario): Unit = {
    val maybeActiveEntity = scenario.matchingEntity(GemMage)
    maybeActiveEntity foreach (e => println(s"""
      |coins: ${e.coins}
      |hp: ${e.hp}
      |treasure: ${e.treasure.mkString(", ")}
    """.stripMargin))
    lazy val activeEntity = maybeActiveEntity.get

    println(padTop)
    scenario.drawn().map(padLeft + _) foreach println
    println(padBottom)

    val directionChars = "uiojkl"

    // change to return RC
    // should be only place case-matching directionChars
    def targetRC = (_: Char) match {
      case 'l' => scenario.moveXYZ(activeEntity, x = 1)
      case 'u' => scenario.moveXYZ(activeEntity, x = -1)
      case 'i' => scenario.moveXYZ(activeEntity, y = 1)
      case 'k' => scenario.moveXYZ(activeEntity, y = -1)
      case 'j' => scenario.moveXYZ(activeEntity, z = 1)
      case 'o' => scenario.moveXYZ(activeEntity, z = -1)
      case c => other(c)
    }

    // refactor to use RC
    def move = (_: Char) match {
      case 'l' => scenario.moveXYZ(activeEntity, x = 1)
      case 'u' => scenario.moveXYZ(activeEntity, x = -1)
      case 'i' => scenario.moveXYZ(activeEntity, y = 1)
      case 'k' => scenario.moveXYZ(activeEntity, y = -1)
      case 'j' => scenario.moveXYZ(activeEntity, z = 1)
      case 'o' => scenario.moveXYZ(activeEntity, z = -1)
      case c => other(c)
    }
    // def attack = (_: Char) match {
      // case 'l' => scenario.harm(activeEntity, x = 1)
      // case 'u' => scenario.harm(activeEntity, x = -1)
      // case 'i' => scenario.harm(activeEntity, y = 1)
      // case 'k' => scenario.harm(activeEntity, y = -1)
      // case 'j' => scenario.harm(activeEntity, z = 1)
      // case 'o' => scenario.harm(activeEntity, z = -1)
      // case c => other(c)
    // }

    def other = (_: Char) match {
      case 'q' => scenario.quit
      case _ => scenario
    }

    lazy val next = stdIn.next() match {
      case c if directionChars.contains(c) => move(c)
      // case 'a' => attack(stdIn.next())
      case c => other(c)
    }

    if (next.resolution.nonEmpty)
      next.resolution foreach println
    // else if (scenario.events.nonEmpty)
        // scenario.tick
    else {
      loop(scenario = next)
    }
  }
  loop(demoScenario)
}
