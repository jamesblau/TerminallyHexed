package terminallyhexed

import Assets._
import Entities._

object `package` {
  type IJ = (Int, Int) // output character (row, column) or offset of same
  type RC = (Int, Int) // hexagon (row, column); implicit w/Hex?
  type Rooms = Set[Int] // multiple for doors
  type Edge = Set[RC] // should only be two -- implicit to enforce?
  type Frames = List[List[String]] // for animations
}

object TerminallyHexed extends App {
  val stdIn = scala.io.Source.stdin

  val playerStarts = Map("A" -> GemMage, "C" -> PsyRat)

  val demoScenario = Demo
    .addPlayers(playerStarts)
    .copy(trapDamage = 3)
    .orderTurns
    .skipMonsterTurns

  val padTop = "\n" * 20
  val padBottom = "\n" * 5
  val padLeft = " " * 1

  def loop(scenario: Scenario): Unit = {
    def printUI = {
      scenario.players.toList.sorted foreach (e => println(List(
        s"${e.name}: hp ${e.hp}/${e.maxHP}",
        s"coins: ${e.coins}",
        s"treasure: ${e.treasure.mkString("[", ", ", "]")}"
      ).mkString(", ")))
      println
      scenario.monsters.toList.sorted foreach (e =>
        println(s"${e.name}: hp ${e.hp}/${e.maxHP}")
      )
    }
    // scenario.frames foreach { frame => printUI; frame foreach println }
    println(padTop)
    printUI
    scenario.drawn().filter(_.trim.nonEmpty).map(padLeft + _) foreach println
    println(padBottom)

    val maybeActivePlayerRC = scenario.activeRC
    val maybeActivePlayer = maybeActivePlayerRC.map(scenario.rc2Entity)
    lazy val activePlayer = maybeActivePlayer.get
    lazy val activePlayerRC = scenario.entity2RC(activePlayer)

    // println("Turn order:"); scenario.turnOrder foreach println
    // println("entity2RC:"); scenario.entity2RC foreach println
    // println("rc2Entity:"); scenario.rc2Entity foreach println
    // println("entities:"); scenario.entities foreach println
    // println("entity2Hex:"); scenario.entity2Hex foreach println
    // println("adjes:"); scenario.adjes foreach println
    // println(s"Maybe active player: ${maybeActivePlayer}")
    // scenario.labelRC.print

    val directionChars = "uiojkl"

    def targetRC(c: Char, rc: RC): RC = c match {
      case 'l' => Hex.goXYZ(rc, x = 1)
      case 'u' => Hex.goXYZ(rc, x = -1)
      case 'i' => Hex.goXYZ(rc, y = 1)
      case 'k' => Hex.goXYZ(rc, y = -1)
      case 'j' => Hex.goXYZ(rc, z = 1)
      case 'o' => Hex.goXYZ(rc, z = -1)
    }

    def move(c: Char): Scenario =
      scenario.move(activePlayer, targetRC(c, scenario.entity2RC(activePlayer)))

    def attack(c: Char, damage: Int = 1): Scenario = {
      if (!directionChars.contains(c))
        scenario
      else {
        val maybeTargetEntity =
          scenario.rc2Entity.get(targetRC(c, scenario.entity2RC(activePlayer)))
        if (maybeTargetEntity.isEmpty)
          scenario
        else
          scenario.harm(maybeTargetEntity.get, damage)
      }
    }

    lazy val next = stdIn.next() match {
      case c if directionChars.contains(c) => move(c)
      case 'a' => attack(stdIn.next())
      case 'n' => scenario.nextTurnOrRound
      case 'q' => scenario.quit
      case _ => scenario
    }

    if (scenario.resolution.nonEmpty) {
      scenario.resolution foreach println
    } else if (maybeActivePlayer.isEmpty) {
      loop(scenario.orderTurns.skipMonsterTurns)
    } else {
      loop(next.clearFrames)
    }
  }
  println
  loop(demoScenario)
}
