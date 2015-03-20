/**
 * Created by bka on 2015-03-16.
 */
package fp

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.{forAll, BooleanOperators}
import scala.util.Random

//8.1
object ExampleScalaCheckSpec extends Properties("List.sum") {

  property("reverse is neutral") = forAll { (xs: List[Int]) =>
    xs.sum == xs.reverse.sum
  }

  property("is mutiplication for list o identical elements") = {
    val const = Gen.const(Random.nextInt(100) + 1)
    forAll(Gen.listOf(const)) { (xs: List[Int]) =>
      (!xs.isEmpty && xs.forall(xs.head == _)) ==> (xs.sum == xs.length * xs.head)
    }
  }

  property("sum of concatenated Lists is sum of their sums") = {
    forAll { (xs: List[Int], ys: List[Int]) =>
      xs.sum + ys.sum == (xs ++ ys).sum
    }
  }

}

//8.2
object ExampleScalaCheckSpec2 extends Properties("List.max") {

  property("reverse is neutral") = forAll { (xs: List[Int]) =>
    !xs.isEmpty ==> (xs.max == xs.reverse.max)
  }

  property("is greater or equal to all elements") = forAll { (xs: List[Int]) =>
    !xs.isEmpty ==> xs.forall(xs.max >= _)
  }

}

object Ch8GenScalaCheckSpec extends Properties("Chapter8.Gen") {
  property("choose gives numbers from given range") = forAll {
    (seed: Long) =>
      val res = Chapter8.Gen.choose(1, 10).sample.run(Chapter6.SimpleRNG(seed))._1
      (res >= 1) :| "result >= " &&
      (res < 10) :| "result < "
  }
}
