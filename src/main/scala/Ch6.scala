/**
 * Created by bka on 2015-02-25.
 */

package fp

object Chapter6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  //6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    val nonNegative = n match {
      case Int.MinValue => 0
      case i => Math.abs(i)
    }
    (nonNegative, nextRNG)
  }

  //6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue.toDouble, nextRNG)
  }

  //6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, nextRNG) = rng.nextInt
    val (d, nextRNG2) = double(nextRNG)
    ((n, d), nextRNG2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, nextRNG) = double(rng)
    val (n, nextRNG2) = nextRNG.nextInt
    ((d, n), nextRNG2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, nextRNG) = double(rng)
    val (d2, nextRNG2) = double(nextRNG)
    val (d3, nextRNG3) = double(nextRNG2)
    ((d1, d2, d3), nextRNG3)
  }

  //6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (Nil, rng)
    case cnt => {
      val (n, nextRNG) = rng.nextInt
      val (tail, finalRNG) = ints(count - 1)(nextRNG)
      (n :: tail, finalRNG)
    }
  }
}