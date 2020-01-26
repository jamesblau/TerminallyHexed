package terminallyhexed

sealed trait Effect {
  val desc: String
}

sealed trait OngoingEffect extends Effect

sealed case class Nerf(desc: String = "A nerf!") extends OngoingEffect

object Frail extends Nerf("+1 harm when struck!")
object Gored extends Nerf("+1 harm per round!")
object Stuck extends Nerf("Can't move!")
object Timid extends Nerf("Can't strike!")
object Smote extends Nerf("Can't anything!")
object Shaky extends Nerf("Disadvantage!")
object Hexed extends Nerf("+1 roll: 0x harm!")

sealed class Buff(val desc: String) extends OngoingEffect

object Lucky extends Buff("Advantage!")
object Fated extends Buff("+1 roll: 2x harm!")

sealed class Inviolable extends Buff("Can't be targeted!")

object Aloof extends Inviolable
object Dodgy extends Inviolable
object Tanky extends Inviolable
object Magic extends Inviolable
object Super extends Inviolable
object Godly extends Inviolable

sealed class ImmediateEffect(val desc: String) extends Effect

object Bump extends ImmediateEffect("Make some space!")
object Yank extends ImmediateEffect("Get over here!")
object Stab extends ImmediateEffect("Ignore armor!")

// unneeded? elsewhere?
case class Rage(extraTargets: Int) extends ImmediateEffect("Who else wants some?")
