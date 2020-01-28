package terminallyhexed

import Assets._

object Asset {
  val wallKeys = "xXyYzZ"
  val upDownEdgeKeys = "ud"
  val nameKeys = "Nn"
  val hudKeys = "AB"
  val edgeKeys = wallKeys + upDownEdgeKeys

  val region2NeighborRegion = Map(
    'x' -> 'X',
    'X' -> 'x',
    'y' -> 'Y',
    'Y' -> 'y',
    'z' -> 'Z',
    'Z' -> 'z',
    'u' -> 'd',
    'd' -> 'u'
  )

  private val padSlice = (_: String).padTo(8, ' ').slice(0,8)

  val region2IJSOffsets: Map[Char, List[IJ]] = """
    |  uuuu       //    u,Y -> 0123
    | xYYYYz      //     x -> 045670 <- z      name NNNNnnnn
    |xxNNNNzz     //         12    12          HUD A, B
    |ZAnnnnBX     //    Z -> 0      0 <- X     Y,y blank for normal, door
    |ZZddddXX     //    d -> 12012312
    |  yyyy       //    y -> 12012312
  |""".tail.stripMargin.split('\n').zipWithIndex.flatMap{ case (line, i) =>
    line.take(8).zipWithIndex.collect { case (c, j) if c != ' ' =>
      (c, (i, j))
    }
  }.groupBy(_._1).view.mapValues(_.map(_._2).toList).toMap

  def apply(s: String) = {
    val lines = s.split('\n').padTo(6, "") map padSlice
    new Asset(
      region2IJSOffsets.view.mapValues(_.map{
        case (j, k) => lines(j)(k)
      }.mkString).toMap.filter(_._2.trim.nonEmpty)
    )
  }
  def apply(N: String, n: String) = new Asset(Map('N' -> N, 'n' -> n))
}

class Asset(val asset: Map[Char, String]) {
  import Asset._
  val names = nameKeys.map(asset.getOrElse(_, "")).toList
  val name = names.map(_.trim).mkString(" ").trim
  val edges = asset.view.filterKeys(edgeKeys.toSet).toMap
  def walls = asset.filter { case (k, v) =>
    wallKeys.contains(k) && Island.contains(k) && v == Island.asset(k)
  }

  val nonEmptyDirections = edges.filter(_._2.trim.nonEmpty).keys.map(_ match {
    case 'u' => 'Y'
    case 'd' => 'y'
    case x => x
  }).toSet.mkString
  val nonVoid = nonEmptyDirections.size == 6

  val isNamed = name.trim.nonEmpty
  val hasNonEdge = (asset -- edgeKeys).values.mkString.trim.nonEmpty
  val hasLowDown = asset.contains('d')
  val isPCStart = isNamed && name.trim.matches("[A-Z]")

  def matches(a: Asset) = a.asset.forall { case (k, v) =>
    asset.contains(k) && asset(k) == v
  }
  def hasDoorEdges = matches(DoorEdges)
  def isIsland = matches(Island)
  def isMoney = matches(Money)
  def isObstacle = matches(Obstacle)
  def isChest = matches(Chest)
  def isTrap = matches(Trap)

  def apply(c: Char): String = asset(c)
  def get(c: Char): Option[String] = asset.get(c)
  def contains(c: Char): Boolean = asset.contains(c)
  def contains(s: String): Boolean = s forall asset.contains
  def cat(s: String) = (s map asset.get).mkString

  override def toString = s"Asset($name)"
  def print() = asset foreach println
}
