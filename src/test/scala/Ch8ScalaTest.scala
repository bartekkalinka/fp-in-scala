package fp

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by bka on 2015-03-20.
 */

class Chapter8Spec extends FlatSpec with Matchers {
  import Chapter8._
  import fp.Chapter6.SimpleRNG

  //8.6
  "listOfN" should "turn int generator into random length list generator" in {
    val genInt = Gen.choose(0, 10)
    val genA = Gen.unit("a")
    val result = genA.listOfN(genInt).sample.run(SimpleRNG(72981098234L))._1
    result.length should be <= 10
    result should contain only ("a")
  }

}