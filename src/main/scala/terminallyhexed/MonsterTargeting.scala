// package terminallyhexed

// object Targeting {
  // def hexesInRange(s: Set[Hex], r: Int): Set[Hex] =
    // if (r == 0) s
    // else hexesInRange(s ++ s.flatMap(_.neighbors), r - 1)

  // val targetsInRange(caster: Entity, range: Int) =
    // hexesInRange(Set(caster.hex), range) flatMap { h => h.occupant }

  // def target = targetsInRange
    // .sortBy{ e => (e.range, e.priority, e.isSummon, e.hex.row, e.hex.col) }
    // .headOption

  // def aoeTargets(target: Entity, attack: Attack) = {
    // if (attack.shape.isOrdered)
     // Nil
    // else {
      // orientations.map(hexes => hexes.filter(!_.isEmpty))
    // }
  // }
// }
