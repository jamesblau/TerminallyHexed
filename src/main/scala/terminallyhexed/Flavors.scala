package terminallyhexed

sealed trait Flavor
sealed trait StrongFlavor extends Flavor
sealed trait WeakFlavor extends Flavor

object Heat extends StrongFlavor
object Cold extends StrongFlavor
object Wind extends StrongFlavor
object Dirt extends StrongFlavor
object Star extends StrongFlavor
object Moon extends StrongFlavor

object Warm extends WeakFlavor
object Cool extends WeakFlavor
object Waft extends WeakFlavor
object Dust extends WeakFlavor
object Noon extends WeakFlavor
object Dusk extends WeakFlavor

object Flavors {
  val strong2Weak = Map(
    Heat -> Warm,
    Cold -> Cool,
    Wind -> Waft,
    Dirt -> Dust,
    Star -> Noon,
    Moon -> Dusk
  )
  val weak2Strong = strong2Weak map { case (k, v) => (v, k) }
}
