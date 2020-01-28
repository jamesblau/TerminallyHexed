package terminallyhexed

case class Entity(
  N: String,
  n: String,
  hp: Int = 0,
  maxHP: Int = 1,
  experience: Int = 0,
  defaultArmor: Int = 0,
  extraArmor: Int = 0,
  isPlayer: Boolean = false,
  effects: Set[Effect] = Set.empty,
  immunities: Set[Effect] = Set.empty,
) {
  assert(N.length == 4 && n.length == 4)
  assert(effects == effects -- immunities)

  val isMonster = !isPlayer
  val names = List(N, n)
  val name = N.trim + " " + n.trim

  def fullMend = this.copy(hp = maxHP)
  def mend(i: Int) = this.copy(hp = maxHP min (hp + i))
  def harm(i: Int) = this.copy(hp = 0 max (hp - i))
  def affect(e: Effect) = this.copy(effects = effects + e)
  def affect(e: Set[Effect]) = this.copy(effects = effects ++ e)

  override def toString = s"Entity($name)"
}

object Entities {
  object GemMage extends Entity("Gem ", "Mage", isPlayer = true)
  object PsyRat extends Entity("Psy ", " Rat", isPlayer = true)

  object BoneGuy extends Entity("Bone", " Guy")
  object BigWolf extends Entity("Big ", "Wolf")
  object FireZila extends Entity("Fire", "Zila")

  val entities = Set(
    GemMage,
    PsyRat,
    BoneGuy,
    BigWolf,
    FireZila,
  )
  val names2Entities = entities.map(e => e.name -> e).toMap
  val names2Players = names2Entities.filter(_._2.isPlayer)
  val names2Monsters = names2Entities.filter(_._2.isMonster)
}
