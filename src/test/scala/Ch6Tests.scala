
/**
 * Created by bka on 2015-02-25.
 */

import org.scalatest.{Matchers, FlatSpec}
import fp.Chapter6._

class Chapter6Spec extends FlatSpec with Matchers {
  "nonNegativeInt" should "return non-negative random numbers" in {
    nonNegativeInt(SimpleRNG(1059025964525L))._1 should be >= 0
  }

  def correctDouble(d: Rand[Double]) = {
    val dbl = d(SimpleRNG(72981098234L))._1
    dbl should be > 0.0
    dbl should be < 1.0
  }

  "double" should "return random number between 0 and 1" in {
    correctDouble(double)
  }

  "intDouble" should "return a different pair from same generator than doubleInt" in {
    val (i, d) = intDouble(SimpleRNG(879234278L))._1
    val (d2, i2) = doubleInt(SimpleRNG(879234278L))._1
    d should be > 0.0
    d should be < 1.0
    d2 should be > 0.0
    d2 should be < 1.0
    i should not be i2
    d should not be d2
  }

  "double3" should "return 3 different random numbers" in {
    val (d1, d2, d3) = double3(SimpleRNG(283373748L))._1
    d1 should not be d2
    d2 should not be d3
    d1 should not be d3
  }

  def correctInts(i: Int => Rand[List[Int]]) = {
    val shortList = i(5)(SimpleRNG(42))._1
    val longerList = i(7)(SimpleRNG(42))._1
    longerList.take(5) should be (shortList)
  }

  "ints" should "produce deterministic list of given length" in {
    correctInts(ints)
  }

  "double2" should "return random number between 0 and 1" in {
    correctDouble(double2)
  }

  "map2" should "combine two random number generator transistions" in {
    map2[Int, Double, Int](_.nextInt, double)((n, d) => (n / d).toInt)(SimpleRNG(42))._1 should be (27079758)
  }

  "sequence" should "combine list of rng transistions into one" in {
    def ir: Rand[Int] = _.nextInt
    def sum = map2(ir, ir)(_ + _)
    def sum4 = map2(sum, sum)(_ + _)
    sequence(List(ir, ir, ir, ir))(SimpleRNG(42))._1.reduce(_ + _) should be (sum4(SimpleRNG(42))._1)
  }

  "ints2" should "produce deterministic list of given length" in {
    correctInts(ints2)
  }

  "nonNegativeLessThan implementations" should "behave the same" in {
    nonNegativeLessThan(100)(SimpleRNG(8332765))._1 should be (nonNegativeLessThan2(100)(SimpleRNG(8332765))._1)
  }

  "mapFlat" should "behave like map" in {
    mapFlat(nonNegativeInt)(i => i - i % 2)(SimpleRNG(723479827))._1 should be (map(nonNegativeInt)(i => i - i % 2)(SimpleRNG(723479827))._1)
  }

  "map2Flat" should "behave like map2" in {
    map2Flat[Int, Double, Int](_.nextInt, double)((n, d) => (n / d).toInt)(SimpleRNG(42))._1 should be (
      map2[Int, Double, Int](_.nextInt, double)((n, d) => (n / d).toInt)(SimpleRNG(42))._1
    )
  }

  "State.map" should "behave like map(Rand)" in {
    State[RNG, Int](nonNegativeInt).map({i: Int => i - i % 2}).run(SimpleRNG(723479827))._1 should be (map(nonNegativeInt)(i => i - i % 2)(SimpleRNG(723479827))._1)
  }

  "State.map2" should "behave like map2(Rand)" in {
    State[RNG, Int](_.nextInt).map2[Double, Int](State(double))((n: Int, d: Double) => (n / d).toInt).run(SimpleRNG(42))._1 should be (
      map2[Int, Double, Int](_.nextInt, double)((n, d) => (n / d).toInt)(SimpleRNG(42))._1
    )
  }

  "State.sequence" should "behave like sequence(Rand)" in {
    def is: State[RNG, Int] = State(_.nextInt)
    def sums: State[RNG, Int] = is.map2(is)(_ + _)
    def sum4s: State[RNG, Int] = sums.map2(sums)(_ + _)
    State.sequence(List(is, is, is, is)).run(SimpleRNG(42))._1.reduce(_ + _) should be (sum4s.run(SimpleRNG(42))._1)
  }

  "simulateMachine" should "return machine transistions according to rules" in {
    simulateMachine(List(Coin, Turn)).run(Machine(true, 1, 0))._2 should be (Machine(true, 0 , 1))
    simulateMachine(List(Turn, Turn, Coin)).run(Machine(true, 2, 0))._2 should be (Machine(false, 2, 1))
    simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Turn)).run(Machine(false, 2, 5))._2 should be (Machine(true, 0, 6))
  }

}