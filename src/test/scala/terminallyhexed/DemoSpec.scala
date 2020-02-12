package terminallyhexed

import org.scalatest.FunSpec
import Assets.Demo
import Entities._

class DemoSpec extends FunSpec {
  describe("Demo") {
    val demoAllRoomsDrawnExpected = """
      |              ▁▁▁▁              |
      |             ╔┴┴┴┴╗             |
      |        ▁▁▁▁╔╝Gem ╚╗▁▁▁▁        |
      |       ╔┴┴┴┴╲ Mage ╱┴┴┴┴╗       |
      |  ▁▁▁▁╔╝     ╲▁▁▁▁╱     ╚╗▁▁▁▁  |
      | ╔┴┴┴┴╲      ╱    ╲      ╱┴┴┴┴╗ |
      |╔╝     ╲▁▁▁▁╱ ╋━━╋ ╲▁▁▁▁╱ Psy ╚╗|
      |╗      ╱    ╲ ╋━━╋ ╱    ╲  Rat ╔|
      |╚╗▁▁▁▁╱ 💀💀 ╲▁▁▁▁╱      ╲▁▁▁▁╔╝|
      | ╔    ╲ ╱╲╱╲ ╔┴┴┴┴╗      ╔┴┴┴┴╗ |
      |╔╝▗╋╋╸ ╲▁▁▁▁╔╝    ╚╗▁▁▁▁╔╝╒╦╦╕╚╗|
      |╗ ▝╋╋1 ╱    ╗      ╔┴┴┴┴╲ ╘══╛ ╔|
      |╚╗▁▁▁▁╱     ╚╗▁▁▁▁╔╝Huge ╲▁▁▁▁╔╝|
      | ╔    ╲      ╱┴┴┴┴╲ Wolf ╱    ╗ |
      |╔╝     ╲▁▁▁▁╱ 💀💀 ╲▁▁▁▁╱     ╚╗|
      |╗      ╱    ╲ ╱╲╱╲ ╱    ╲      ╔|
      |╚╗▁▁▁▁╱ Bone ╲▁▁▁▁╱      ╲▁▁▁▁╔╝|
      |  ┴┴┴┴╗  Guy ╱    ╲      ╔┴┴┴┴  |
      |      ╚╗▁▁▁▁╱      ╲▁▁▁▁╔╝      |
      |        ┴┴┴┴╗      ╔┴┴┴┴        |
      |            ╚╗    ╔╝            |
      |             ╔╤╤╤╤╗             |
      |             ╠╪╪╪╪╣             |
      |             ╠╪╪╪╪╣             |
      |             ╠╪╪╪╪╣             |
      |             ╔    ╗             |
      |        ▁▁▁▁╔╝    ╚╗▁▁▁▁        |
      |       ╔┴┴┴┴╲      ╱┴┴┴┴╗       |
      |      ╔╝     ╲▁▁▁▁╱     ╚╗      |
      |      ╗      ╱    ╲      ╔      |
      |      ╚╗▁▁▁▁╱ Fire ╲▁▁▁▁╔╝      |
      |        ┴┴┴┴╗ Zila ╔┴┴┴┴        |
      |            ╚╗▁▁▁▁╔╝            |
      |              ┴┴┴┴              |
    |""".tail.stripMargin.split('\n').map(_.init).toList

    val demoRoomZeroDrawnExpected = {
      val (roomZero, roomOne) = demoAllRoomsDrawnExpected.splitAt(25)
      roomZero ++ roomOne.map(_.map(_ => ' '))
    }

    it("should load Demo scenario, add players, and print first room as expected") {
      val playerStarts = Map("A" -> GemMage, "C" -> PsyRat)
      val output: List[String] = Demo.addPlayers(playerStarts).drawn()
      (output zip demoRoomZeroDrawnExpected).foreach {
        case (outLine, expectedLine) => assert(outLine == expectedLine)
      }
    }
  }
}
