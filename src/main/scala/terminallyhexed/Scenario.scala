package terminallyhexed

import Assets._

case class Scenario(
  name: String,
  winConditions: String,
  height: Int,
  width: Int,
  hexes: Set[Hex],
  entity2RC: Map[Entity, RC],
  rc2Treasure: Map[RC, String] = Map.empty,
  turnOrder: List[RC] = Nil,
  trapDamage: Int = 1,
  adjes: Map[RC, Set[RC]] = Map.empty,
  openRooms: Set[Int] = Set(0),
  frames: Frames = Nil,
  resolution: Option[String] = None,
) {
  lazy val rc2Hex: Map[RC, Hex] = hexes.map(h => (h.rc, h)).toMap
  lazy val rc2Entity: Map[RC, Entity] = entity2RC.map { case (k, v) => (v, k) }
  lazy val entity2Hex: Map[Entity, Hex] = entity2RC.view.mapValues(rc2Hex).toMap
  lazy val hex2Entity: Map[Hex, Entity] = entity2Hex.map { case (k, v) => (v, k) }

  lazy val entities: Set[Entity] = entity2RC.keys.toSet
  lazy val players: Set[Entity] = entities.filter(_.isPlayer)
  lazy val monsters: Set[Entity] = entities -- players
  lazy val activeRC: Option[RC] = turnOrder.headOption

  lazy val rooms: Set[Int] = hexes.flatMap(_.rooms)

  lazy val nodes: Set[RC] = hexes.map(_.rc)
  lazy val edges: Set[Edge] = adjes.flatMap { case (from, set) =>
    set.map(to => Set(from, to))
  }.toSet

  lazy val openHexes = hexes.filter(_.rooms.filter(openRooms).nonEmpty)

  // Setup
  def addPlayers(playerStarts: Map[String, Entity]) = {
    val start2RC = hexes.flatMap(h =>
      h.start.map(s => s -> h.rc).filterNot(_ => h.isDoor)
    ).toMap
    val newEntities2RC = playerStarts.map { case (s, e) => e -> start2RC(s) }
    val allEntities2RC = (entity2RC ++ newEntities2RC).map {
      case (e, rc) => e.fullMend -> rc
    }
    this.copy(entity2RC = allEntities2RC)
  }
  def buildAdjes = this.copy(adjes = rc2Hex.view.mapValues(h =>
    h.edge2AdjacentRC.toList.collect{ case (e, rc2)
      if rc2Hex.contains(rc2) && h.get(e).map(_ != Island(e)).getOrElse(true)
      => rc2
    }.toSet
  ).toMap)

  // Turns
  def orderTurns = this.copy(
    turnOrder = entity2RC.toList.sortBy(_._1.initiative).map(_._2)
  )
  def skipMonsterTurns = this.copy(
    turnOrder = turnOrder.filter(rc2Entity(_).isPlayer)
  )
  def nextTurnOrRound = if (turnOrder.isEmpty) this.orderTurns
    else this.copy(turnOrder = turnOrder.tail)

  // Graph
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
    h.copy(rooms = conComps.collect{ case (c, nodes) if nodes(h.rc) => c }.toSet)
  ))
  def areColinear(s: Set[RC]) = ???

  // Entities

  def maybeMatchingEntity(e: Entity) = entity2RC.keys.find(_.names == e.names)
  def isOccupied(rc: RC) =
    entity2RC.values.toList.contains(rc) || rc2Hex(rc).isObstacle

  def buryDead = {
    val deadEntity2RC = entity2RC.filter(_._1.hp <= 0)
    val (deadEntities, deadRCs) = deadEntity2RC.toList.unzip
    val deadPlayerHexes = deadEntity2RC.filter(_._1.isPlayer).values.map(rc2Hex)
    val deadMonsterHexes = deadEntity2RC.filter(_._1.isMonster).values.map(rc2Hex)
    val deadHexes = deadPlayerHexes ++ deadMonsterHexes
    val clearedHexes = deadPlayerHexes.map(_.clearNames)
    val coinyHexes = deadMonsterHexes.map(_.withCoin(1))
    this.copy(
      hexes = hexes -- deadHexes ++ clearedHexes ++ coinyHexes,
      entity2RC = entity2RC -- deadEntities,
      turnOrder = turnOrder.filterNot(deadRCs.contains)
    ).maybeResolve.addFrame
  }

  def mapEntity(e: Entity, f: Entity => Entity) =
    this.copy(entity2RC = entity2RC - e + (f(e) -> entity2RC(e)))
  def mend(e: Entity, i: Int) = mapEntity(e, _.mend(i))
  def harm(e: Entity, i: Int) = mapEntity(e, _.harm(i)).buryDead
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
        hexes = hexes - end + deDoor(end.looted),
        entity2RC = entity2RC - e + (updatedEntity -> endRC),
        turnOrder = endRC :: turnOrder.tail,
        openRooms = openRooms ++ maybeNewRooms,
      ).harm(updatedEntity, damage).addFrame
    } else this
  }
  def moveXYZ(e: Entity, x: Int = 0, y: Int = 0, z: Int = 0) =
    move(e, entity2Hex(e).goXYZ(x = x, y = y, z = z))

  // Resolution
  def win = this.copy(resolution = Some("Victory!"))
  def lose = this.copy(resolution = Some("Defeat!"))
  def quit = this.copy(resolution = Some("You gave up..."))
  def maybeResolve = if (players.isEmpty) this.lose
    else if (winConditions == "c" && monsters.isEmpty) this.win
    else this

  // Graphics
  def reWall(h: Hex) = h.edge2AdjacentRC.foldLeft(h) { case (h, (edge, rc)) =>
    h.copy(asset = {
      if (adjes(h.rc).contains(rc))
        h.asset + (edge -> Normal.asset.get(edge).getOrElse("    "))
      else h.asset + (edge -> Island.asset(edge))
    } )
  }
  def deDoor(h: Hex) = if (!h.isDoor) h else reWall(h.copy(asset = Island.asset))
  def reWallAll = this.copy(hexes = hexes.map(reWall))
  def clearHuds = this.copy(hexes = hexes.map {
    case h if h.isDoor => h.copy(asset = h.asset ++ Door.asset)
    case h => h.copy(asset = h.asset -- Asset.hudKeys.toList)
  } )
  lazy val blank = List.fill(height)(" " * width)
  def drawn(hs: Set[Hex] = openHexes) = hs.map(h =>
    hex2Entity.get(h).map(e => h.setNames(e.names)).getOrElse(h.labelCoinCount)
  ).foldLeft(blank) { case (l, hex) =>
    hex.charIJS.foldLeft(l) { case (ll, (char, (i, j))) =>
      ll.updated(i, ll(i).updated(j, char))
    }
  }
  def addFrame = this.copy(frames = drawn() :: frames)
  def clearFrames = this.copy(frames = Nil)
  def print = drawn() foreach println
  override def toString = s"Scenario($name)"

  // Debug
  def labelRC = this.copy(hexes = hexes.map { h =>
    h.copy(asset = h.asset ++ Map('A' -> h.r.toString, 'B' -> h.c.toString))
  } )
  def labelRooms = this.copy(hexes = hexes.map { h =>
    val List(a, b) = h.rooms.toList.sorted.map(_.toString).padTo(2, " ")
    h.copy(asset = h.asset ++ Map('A' -> a, 'B' -> b))
  } )
}
