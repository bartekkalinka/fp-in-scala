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
}