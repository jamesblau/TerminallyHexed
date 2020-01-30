package terminallyhexed

import Assets._

object Scenario {
  def fromString(
    name: String,
    winConditions: String,
    rc2Treasure: Map[RC, String],
    string: String
  ) = {
    val unPadded = string.split('\n').toList
    val (height, width) = (unPadded.size, unPadded.map(_.size).max)
    val board = unPadded.map(_.padTo(width, ' '))

    val offset = (board.head.takeWhile(_ == ' ').size - 2) % 12 / 6
    val hexRows = (height + 2 * offset - 1) / 4
    val hexCols = (width - 2) / 6

    def loadAsset(h: Hex): Hex = h.copy(asset =
      h.region2IJS.view.mapValues(_.map{
        case (i, j) => board(i)(j)
      }.mkString).filter(_._2.trim.nonEmpty).toMap
    )

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
        ).initLoot(rc2Treasure)
    } ).toMap

    val hexes = rc2Hex.values.toSet

    val entity2RC = Entities.entities.flatMap(e =>
      hexes.find(_.names.head == e.names.head).map(e -> _.rc)
    ).toMap

    new Scenario(
      name,
      winConditions,
      height,
      width,
      hexes,
      entity2RC,
      rc2Treasure
    )
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
  entity2RC: Map[Entity, RC],
  rc2Treasure: Map[RC, String] = Map.empty,
  trapDamage: Int = 1,
  adjes: Map[RC, Set[RC]] = Map.empty,
  openRooms: Set[Int] = Set(0),
  resolution: Option[String] = None,
) {
  lazy val rc2Hex: Map[RC, Hex] = hexes.map(h => (h.rc, h)).toMap
  lazy val rc2Entity: Map[RC, Entity] = entity2RC.map { case (k, v) => (v, k) }
  lazy val entity2Hex: Map[Entity, Hex] = entity2RC.view.mapValues(rc2Hex).toMap
  lazy val hex2Entity: Map[Hex, Entity] = entity2Hex.map { case (k, v) => (v, k) }

  lazy val entities: Set[Entity] = entity2RC.keys.toSet
  lazy val players: Set[Entity] = entities.filter(_.isPlayer)
  lazy val monsters: Set[Entity] = entities -- players

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
  def addPlayers(starts: Map[String, Entity]) = {
    val start2RC = hexes.flatMap(h =>
      h.start.map(s => s -> h.rc).filterNot(_ => h.isDoor)
    ).toMap
    val newEntities2RC = starts.map { case (s, e) => e -> start2RC(s) }
    val allEntities = (entity2RC ++ newEntities2RC).map {
      case (e, rc) => e.fullMend -> rc
    }
    this.copy(entity2RC = allEntities)
  }

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

  def matchingEntity(e: Entity) = entity2RC.keys.find(_.names == e.names)

  def isOccupied(rc: RC) =
    entity2RC.values.toList.contains(rc) || rc2Hex(rc).isObstacle

  def checkResolution =
    if (players.isEmpty)
      this.lose
    else if (winConditions == "c" && monsters.isEmpty)
      this.win
    else
      this
  def filterDead = this.copy(
    entity2RC = entity2RC.view.filterKeys(_.hp > 0).toMap
  ).checkResolution

  def mapEntity(e: Entity, f: Entity => Entity) =
    this.copy(entity2RC = entity2RC - e + (f(e) -> entity2RC(e)))
  def mend(e: Entity, i: Int) = mapEntity(e, _.mend(i))
  def harm(e: Entity, i: Int) = mapEntity(e, _.harm(i)).filterDead
  def affect(e: Entity, eff: Effect) = mapEntity(e, _.affect(eff))
  def affect(e: Entity, s: Set[Effect]) = s.foldLeft(this) {
    case (scenario, eff) => scenario.mapEntity(e, _.affect(eff))
  }

  def move(e: Entity, endRC: RC) = {
    val start = entity2Hex(e)
    if (adjes(start.rc)(endRC) && !isOccupied(endRC)) {
      val end = rc2Hex(endRC)
      val maybeNewRooms = end.rooms
      val damage = if (end.isTrap) trapDamage else 0
      val updatedEntity = e.getCoins(end.coins).getTreasure(end.treasure)
      this.copy(
        entity2RC = entity2RC - e + (updatedEntity -> endRC),
        openRooms = openRooms ++ maybeNewRooms,
        hexes = hexes - end + end.looted
      ).harm(updatedEntity, damage)
    } else this
  }
  def moveXYZ(e: Entity, x: Int = 0, y: Int = 0, z: Int = 0) =
    move(e, entity2Hex(e).goXYZ(x = x, y = y, z = z))

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

  def win = this.copy(resolution = Some("Victory!"))
  def lose = this.copy(resolution = Some("Defeat!"))
  def quit = this.copy(resolution = Some("You gave up..."))

  lazy val blank = List.fill(height)(" " * width)
  def drawn(hs: Set[Hex] = openHexes) = hs.map(h =>
    hex2Entity.get(h).map(e => h.setNames(e.names)).getOrElse(h.labelCoinCount)
  ).foldLeft(blank) { case (l, hex) =>
    hex.charIJS.foldLeft(l) { case (ll, (char, (i, j))) =>
      ll.updated(i, ll(i).updated(j, char))
    }
  }
  def print = drawn() foreach println
  override def toString = s"Scenario($name)"
}
