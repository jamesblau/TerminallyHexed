package terminallyhexed

object Assets {
  val Normal = Asset("""
    |  ▁▁▁▁
    | ╱    ╲
    |╱      ╲
    |╲      ╱
    | ╲▁▁▁▁╱
  |""".tail.stripMargin)
  val Island = Asset("""
    |  ▁▁▁▁
    | ╔┴┴┴┴╗
    |╔╝    ╚╗
    |╗      ╔
    |╚╗▁▁▁▁╔╝
    |  ┴┴┴┴
  |""".tail.stripMargin)
  val Door = Asset("""
    |
    | ╔╤╤╤╤╗
    | ╠╪╪╪╪╣
    | ╠╪╪╪╪╣
    | ╠╪╪╪╪╣
  |""".tail.stripMargin)
  val DoorEdges = Asset("""
    |
    | ╔╤╤╤╤╗
    | ╠    ╣
    |
    | ╠╪╪╪╪╣
  |""".tail.stripMargin)
  val Coin = Asset(
    "▗╋╋╸",
    "▝╋╋╸"
  )
  val Obstacle = Asset(
    "╋━━╋",
    "╋━━╋"
  )
  val Treasure = Asset(
    "╒╦╦╕",
    "╘══╛"
  )
  val Trap = Asset(
    "💀💀",
    "╱╲╱╲"
  )
  val Demo = ScenarioBuilders.fromString(
    "Demo",
    "c",
    Map((2, 4) -> "Map of Folding"),
    """
      |              ▁▁▁▁
      |             ╔┴┴┴┴╗
      |        ▁▁▁▁╔╝    ╚╗▁▁▁▁
      |       ╔┴┴┴┴╲0    A╱┴┴┴┴╗
      |  ▁▁▁▁╔╝     ╲▁▁▁▁╱     ╚╗▁▁▁▁
      | ╔┴┴┴┴╲      ╱    ╲     B╱┴┴┴┴╗
      |╔╝     ╲▁▁▁▁╱ ╋━━╋ ╲▁▁▁▁╱     ╚╗
      |╗      ╱    ╲ ╋━━╋ ╱    ╲     C╔
      |╚╗▁▁▁▁╱ 💀💀 ╲▁▁▁▁╱      ╲▁▁▁▁╔╝
      | ╔    ╲ ╱╲╱╲ ╔┴┴┴┴╗     D╔┴┴┴┴╗
      |╔╝▗╋╋╸ ╲▁▁▁▁╔╝    ╚╗▁▁▁▁╔╝╒╦╦╕╚╗
      |╗ ▝╋╋╸ ╱    ╗      ╔┴┴┴┴╲ ╘══╛ ╔
      |╚╗▁▁▁▁╱     ╚╗▁▁▁▁╔╝Huge ╲▁▁▁▁╔╝
      | ╔    ╲      ╱┴┴┴┴╲ Wolf ╱    ╗
      |╔╝     ╲▁▁▁▁╱ 💀💀 ╲▁▁▁▁╱     ╚╗
      |╗      ╱    ╲ ╱╲╱╲ ╱    ╲      ╔
      |╚╗▁▁▁▁╱ Bone ╲▁▁▁▁╱      ╲▁▁▁▁╔╝
      |  ┴┴┴┴╗  Guy ╱    ╲      ╔┴┴┴┴
      |      ╚╗▁▁▁▁╱ 💀💀 ╲▁▁▁▁╔╝
      |        ┴┴┴┴╗ ╱╲╱╲ ╔┴┴┴┴
      |            ╚╗    ╔╝
      |             ╔╤╤╤╤╗
      |             ╠╪╪╪╪╣
      |             0╪╪╪╪1
      |             ╠╪╪╪╪╣
      |             ╔    ╗
      |        ▁▁▁▁╔╝    ╚╗▁▁▁▁
      |       ╔┴┴┴┴╲1     ╱┴┴┴┴╗
      |      ╔╝     ╲▁▁▁▁╱     ╚╗
      |      ╗      ╱    ╲      ╔
      |      ╚╗▁▁▁▁╱ Fire ╲▁▁▁▁╔╝
      |        ┴┴┴┴╗ Zila ╔┴┴┴┴
      |            ╚╗▁▁▁▁╔╝
      |              ┴┴┴┴
    |""".tail.stripMargin
  )
}
