package terminallyhexed

import Assets._

object Scenario {
  def fromString(string: String) = {
    val name :: winConditions :: unPadded = string.split('\n').toList
    val (height, width) = (unPadded.size, unPadded.map(_.size).max)
    val board = unPadded.map(_.padTo(width, ' '))

    val offset = (board.head.takeWhile(_ == ' ').size - 2) % 12 / 6
    val hexRows = (height + 2 * offset - 1) / 4
    val hexCols = (width - 2) / 6

    def loadAsset(h: Hex): Hex = {
      val withAsset = h.copy(asset =
        h.region2IJS.view.mapValues(_.map{
          case (i, j) => board(i)(j)
        }.mkString).filter(_._2.trim.nonEmpty).toMap
      )
      withAsset.fill(Entities.names2Entities.get(withAsset.name))
    }

    val unlabeledHexes = (0 until hexRows).flatMap(r =>
      (0 until hexCols) map (Hex(r, _, offset))
    ).filter(h =>
      height >= h.i0 + 5 && h.i0 >= 0
    ).map(loadAsset).filter(h =>
      !h.isIsland && h.nonVoid
    )

    val rc2Hex = unlabeledHexes.map(h => h.rc -> {
      if (h.hasDoorEdges)
        h.copy(rooms = Set(h('A').toInt, h('B').toInt))
      else
        h.copy(
          asset = h.asset.filter { case (k, v) =>
            !Door.asset.values.toList.contains(v)
          }, // so doors erase fully
          rooms = h.get('A').map(_.toInt).toSet,
          start = h.get('B')
        )
    } ).toMap

    val hexes: Set[Hex] = rc2Hex.values.toSet

    new Scenario(name, winConditions, height, width, hexes)
      .buildAdjes
      .propagateRooms
      .clearHuds
  }
}

case class Scenario(
  name: String,
  winConditions: String,
  height: Int,
  width: Int,
  hexes: Set[Hex],
  activeEntity: Option[Entity] = None,
  adjes: Map[RC, Set[RC]] = Map.empty,
  openRooms: Set[Int] = Set(0),
  won: Boolean = false,
) {
  lazy val rc2Hex: Map[RC, Hex] = hexes.map(h => (h.rc, h)).toMap

  lazy val entities: Set[Entity] = hexes.flatMap(_.occupant).toSet
  lazy val players: Set[Entity] = entities.filter(_.isPlayer)
  lazy val monsters: Set[Entity] = entities -- players

  lazy val entity2Hex: Map[Entity, Hex] = hexes.collect {
    case h if h.hasEntity => h.occupant.get -> h
  }.toMap
  lazy val entity2RC: Map[Entity, RC] = entity2Hex.view.mapValues(_.rc).toMap
  lazy val rooms: Set[Int] = hexes.flatMap(_.rooms)

  lazy val nodes: Set[RC] = hexes.map(_.rc)
  lazy val edges: Set[Edge] = adjes.flatMap { case (from, set) =>
    set.map(to => Set(from, to))
  }.toSet

  lazy val openHexes = hexes.filter(_.rooms.filter(openRooms).nonEmpty)
  lazy val openNodes = openHexes.map(_.rc)

  def buildAdjes = this.copy(adjes = rc2Hex.view.mapValues(h =>
    h.edge2AdjacentRC.toList.collect{ case (e, rc2)
      if rc2Hex.contains(rc2) && h.get(e).map(_ != Island(e)).getOrElse(true)
      => rc2
    }.toSet
  ).toMap)
  def addPlayers(starts: Map[String, Entity]) = this.copy(
    hexes = hexes.map(h => h.start match {
      case Some(room) if starts.contains(room) => h.fill(Some(starts(room)))
      case _ => h
    } )
  )

  def connectedComponent(nodes: Set[RC]): Set[RC] = {
    val children = nodes.filterNot(rc2Hex(_).isDoor).flatMap(adjes) -- nodes
    children.toList match {
      case Nil => nodes
      case _ => connectedComponent(children.foldLeft(nodes) {
        case (comp, child) => comp + child
      } )
    }
  }
  lazy val conComps: Map[Int, Set[RC]] = rooms.map(room => {
    val h = hexes.find(h => h.rooms == Set(room)).get
    room -> connectedComponent(Set(h.rc))
  } ).toMap
  def propagateRooms = this.copy(hexes = hexes.map(h =>
    h.copy(rooms = conComps.collect{
      case (c, nodes) if nodes(h.rc) => c
    }.toSet)
  ))

  def areColinear(s: Set[RC]) = ???

  def setActive(e: Option[Entity]) = this.copy(activeEntity = e)
  def move(x: Int = 0, y: Int = 0, z: Int = 0) = {
    val start = entity2Hex(activeEntity.get)
    val (r, c) = start.rc
    val endRC = Hex.goXYZ(r, c, x = x, y = y, z = z)
    if (adjes(r, c)(endRC) && rc2Hex(endRC).unoccupied) {
      val end = rc2Hex(endRC)
      val maybeNewRooms = end.rooms
      this.copy(
        hexes = hexes - start - end + start.empty + end.fill(activeEntity),
        openRooms = openRooms ++ maybeNewRooms,
      )
      // val path = ???
      // this.copy(events = events.push(Moving(path): Event))
    } else this
  }

  // def moving(p: Path) = {
    // val next = path.head
    // val maybeNewRooms = next.rooms
    // val filled = next.fill(Some(activeEntity).get)
    // this.copy(
      // hexes = hexes - start - next + start.empty + filled,
      // openRooms = openRooms ++ maybeNewRooms,
    // )
  // }

  def clearHuds = this.copy(hexes = hexes.map {
    case h if h.isDoor => h.copy(asset = h.asset ++ Door.asset)
    case h => h.copy(asset = h.asset -- Asset.hudKeys.toList)
  } )
  def labelRC = this.copy(hexes = hexes.map { h =>
    h.copy(asset = h.asset ++ Map('A' -> h.r.toString, 'B' -> h.c.toString))
  } )
  def labelRooms = this.copy(hexes = hexes.map { h =>
    val List(a, b) = h.rooms.toList.sorted.map(_.toString).padTo(2, " ")
    h.copy(asset = h.asset ++ Map('A' -> a, 'B' -> b))
  } )

  def win = this.copy(won = true)

  lazy val blank = List.fill(height)(" " * width)
  def drawn(hs: Set[Hex] = openHexes) = hs.foldLeft(blank) { case (l, hex) =>
    hex.charIJS.foldLeft(l) { case (ll, (char, (i, j))) =>
      ll.updated(i, ll(i).updated(j, char))
    }
  }
  override def toString = s"Scenario($name)"
}
