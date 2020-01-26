package terminallyhexed

import org.scalatest.FunSpec
import Assets.Demo
import Entities._

class DemoSpec extends FunSpec {
  describe("Demo") {
    val demoRoomZeroDrawnExpected = """
      |              ▁▁▁▁              |
      |             ╔┴┴┴┴╗             |
      |        ▁▁▁▁╔╝Gem ╚╗▁▁▁▁        |
      |       ╔┴┴┴┴╲ Mage ╱┴┴┴┴╗       |
      |  ▁▁▁▁╔╝     ╲▁▁▁▁╱     ╚╗▁▁▁▁  |
      | ╔┴┴┴┴╲      ╱    ╲      ╱┴┴┴┴╗ |
      |╔╝     ╲▁▁▁▁╱ ╋━━╋ ╲▁▁▁▁╱ Psy ╚╗|
      |╗      ╱    ╲ ╋━━╋ ╱    ╲  Rat ╔|
      |╚╗▁▁▁▁╱      ╲▁▁▁▁╱      ╲▁▁▁▁╔╝|
      | ╔    ╲      ╔┴┴┴┴╗      ╔┴┴┴┴╗ |
      |╔╝▗╋╋╸ ╲▁▁▁▁╔╝    ╚╗▁▁▁▁╔╝╒╦╦╕╚╗|
      |╗ ▝╋╋╸ ╱    ╗      ╔┴┴┴┴╲ ╘══╛ ╔|
      |╚╗▁▁▁▁╱     ╚╗▁▁▁▁╔╝Big  ╲▁▁▁▁╔╝|
      | ╔    ╲      ╱┴┴┴┴╲ Wolf ╱    ╗ |
      |╔╝     ╲▁▁▁▁╱ 💀💀 ╲▁▁▁▁╱     ╚╗|
      |╗      ╱    ╲ ╱╲╱╲ ╱    ╲      ╔|
      |╚╗▁▁▁▁╱ Bone ╲▁▁▁▁╱      ╲▁▁▁▁╔╝|
      |  ┴┴┴┴╗ Guy  ╱    ╲      ╔┴┴┴┴  |
      |      ╚╗▁▁▁▁╱      ╲▁▁▁▁╔╝      |
      |        ┴┴┴┴╗      ╔┴┴┴┴        |
      |            ╚╗    ╔╝            |
      |             ╔╤╤╤╤╗             |
      |             ╠╪╪╪╪╣             |
      |             ╠╪╪╪╪╣             |
      |             ╠╪╪╪╪╣             |
      |                                |
      |                                |
      |                                |
      |                                |
      |                                |
      |                                |
      |                                |
      |                                |
      |                                |
    |""".tail.stripMargin.split('\n').map(_.init).toList

    val demoAllRoomsDrawnExpected = """
      |              ▁▁▁▁              |
      |             ╔┴┴┴┴╗             |
      |        ▁▁▁▁╔╝Gem ╚╗▁▁▁▁        |
      |       ╔┴┴┴┴╲ Mage ╱┴┴┴┴╗       |
      |  ▁▁▁▁╔╝     ╲▁▁▁▁╱     ╚╗▁▁▁▁  |
      | ╔┴┴┴┴╲      ╱    ╲      ╱┴┴┴┴╗ |
      |╔╝     ╲▁▁▁▁╱ ╋━━╋ ╲▁▁▁▁╱ Psy ╚╗|
      |╗      ╱    ╲ ╋━━╋ ╱    ╲  Rat ╔|
      |╚╗▁▁▁▁╱      ╲▁▁▁▁╱      ╲▁▁▁▁╔╝|
      | ╔    ╲      ╔┴┴┴┴╗      ╔┴┴┴┴╗ |
      |╔╝▗╋╋╸ ╲▁▁▁▁╔╝    ╚╗▁▁▁▁╔╝╒╦╦╕╚╗|
      |╗ ▝╋╋╸ ╱    ╗      ╔┴┴┴┴╲ ╘══╛ ╔|
      |╚╗▁▁▁▁╱     ╚╗▁▁▁▁╔╝Big  ╲▁▁▁▁╔╝|
      | ╔    ╲      ╱┴┴┴┴╲ Wolf ╱    ╗ |
      |╔╝     ╲▁▁▁▁╱ 💀💀 ╲▁▁▁▁╱     ╚╗|
      |╗      ╱    ╲ ╱╲╱╲ ╱    ╲      ╔|
      |╚╗▁▁▁▁╱ Bone ╲▁▁▁▁╱      ╲▁▁▁▁╔╝|
      |  ┴┴┴┴╗ Guy  ╱    ╲      ╔┴┴┴┴  |
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

    it("should load Demo scenario, add players, and print first room as expected") {
      val playerStarts = Map("A" -> GemMage, "C" -> PsyRat)
      val output = Demo.addPlayers(playerStarts).drawn()
      assert(output == demoRoomZeroDrawnExpected)
    }
  }
}
