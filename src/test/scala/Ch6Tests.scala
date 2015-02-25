/**
 * Created by bka on 2015-02-25.
 */

import org.scalatest.{Matchers, FlatSpec}
import fp.Chapter6._

class Chapter6Spec extends FlatSpec with Matchers {
  "nonNegativeInt" should "return non-negative random numbers" in {
    nonNegativeInt(SimpleRNG(1059025964525L))._1 should be >= 0
  }

  "double" should "return random number between 0 and 1" in {
    val dbl = double(SimpleRNG(72981098234L))._1
    dbl should be > 0.0
    dbl should be < 1.0
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

  "ints" should "produce deterministic list of given length" in {
    val shortList = ints(5)(SimpleRNG(42))._1
    val longerList = ints(7)(SimpleRNG(42))._1
    longerList.take(5) should be (shortList)
  }
}