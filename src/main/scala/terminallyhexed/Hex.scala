package terminallyhexed

import Assets._

object Hex {
  def rc2XYZ(r: Int, c: Int) = {
    val z = r - (c - (c & 1)) / 2
    (c, -c - z, z)
  }
  def rc2I0J0(r: Int, c: Int, offset: Int) = (
    4 * r + 2 * ((c & 1) - offset),
    6 * c
  )

  def xz2RC(x: Int, z: Int) = (
    z + (x - (x & 1)) / 2,
    x
  )
  def i0j02RC(i0: Int, j0: Int, offset: Int) = (
    (i0 - 2 * (((j0 / 6) & 1) - offset)) / 4,
    j0 / 6,
  )

  def fromXZ(x: Int, z: Int, offset: Int = 0) = {
    val (r, c) = xz2RC(x, z)
    new Hex(r, c, offset)
  }
  def fromI0J0(i0: Int, j0: Int, offset: Int = 0) = {
    val (r, c) = i0j02RC(i0, j0, offset)
    new Hex(r, c, offset)
  }

  def goXYZ(rc: RC, x: Int = 0, y: Int = 0, z: Int = 0) = {
    val (x0, _, z0) = rc2XYZ(rc._1, rc._2)
    xz2RC(x0 + x - z, z0 + z - y)
  }
}
case class Hex(
  r: Int = 0,
  c: Int = 0,
  offset: Int = 0,
  coins: Int = 0,
  treasure: Option[String] = None,
  rooms: Set[Int] = Set.empty,
  start: Option[String] = None,
  override val asset: Map[Char, String] = Map.empty,
) extends Asset(asset) {
  import Hex._
  assert(rooms.size < 3)

  val (x, y, z) = rc2XYZ(r, c)
  val (i0, j0) = rc2I0J0(r, c, offset)
  val rc = (r, c)

  def distance(h: Hex) = ((x - h.x).abs + (y - h.y).abs + (z - h.z).abs) / 2

  def goXYZ(x: Int = 0, y: Int = 0, z: Int = 0) = Hex.goXYZ(rc, x, y, z)

  val edge2AdjacentRC: Map[Char, RC] = Map(
    'X' -> goXYZ(x = 1),
    'x' -> goXYZ(x = -1),
    'Y' -> goXYZ(y = 1),
    'y' -> goXYZ(y = -1),
    'Z' -> goXYZ(z = 1),
    'z' -> goXYZ(z = -1)
  )

  val region2IJS: Map[Char, List[IJ]] =
    Asset.region2IJSOffsets.view.mapValues(_.map {
      case (i, j) => (i + i0, j + j0)
    }).toMap
  val region2CharIJS: Map[Char, List[(Char, IJ)]] =
    region2IJS.collect { case (region, ijs) if contains(region) =>
      (region, (asset(region) zip ijs).toList)
    }
  val charIJS: List[(Char, IJ)] =
    region2CharIJS.values.flatten.toList

  val isDoor = rooms.size > 1

  def setNames(names: List[String]) = this.copy(
    asset = asset + ('N' -> names.head) + ('n' -> names.last)
  )
  def clearNames = setNames(List("", ""))

  def initLoot(rc2Treasure: Map[RC, String]) =
    if (isCoin)
      this.copy(coins = 1)
    else
      this.copy(treasure = rc2Treasure.find(_._1 == rc).map(_._2))
  def labelCoinCount =
    if (isCoin) setNames(List(names.head, names.last.init + coins.toString))
    else this

  def looted = this.copy(coins = 0, treasure = None).clearNames
  def withCoin(i: Int = 1) = this.copy(
    coins = coins + i,
    asset = asset ++ Coin.asset
  )

  override def toString = s"Hex($r,$c: ${name})"
  override def print = println(toString)
}
