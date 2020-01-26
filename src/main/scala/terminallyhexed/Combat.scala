package terminallyhexed

sealed trait Shape

sealed trait Event

case class Skill(events: List[Event])

case class Imbibe(flavors: Set[Flavor], effect: Effect) extends Event

case class Decant(flavors: Set[Flavor]) extends Event

case class Strike(
  mend: Int = 0,
  harm: Int = 0,
  range: Int = 1,
  pull: Int = 0,
  targets: Int = 0,
  shape: Option[Shape] = None,
  casterEffects: List[Effect] = Nil,
  targetEffects: List[Effect] = Nil,
  imbibes: List[Imbibe] = Nil,
  isMelee: Boolean = true,
) extends Event {
  def resolve(d: Dice) = ???
}

case class Sortie(attacks: List[Strike])


case class Roll(
  add: Int = 0,
  multiply: Int = 1,
  rolling: Boolean = false,
  raisedFlavors: Set[Flavor] = Set.empty,
)

case class Dice(rolls: List[Roll])
