package terminallyhexed

import Assets.Door

object ScenarioBuilders {
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
