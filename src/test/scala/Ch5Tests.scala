/**
 * Created by bka on 2015-02-13.
 */

import org.scalatest.{Matchers, FlatSpec}
import fp.Chapter5._

class Chapter5Spec extends FlatSpec with Matchers {

  "toList" should "force stream evaluation" in {
    Stream(1, 2, 3, 4, 5, 6, 7).toList should be (List(1, 2, 3, 4, 5, 6, 7))
  }

  "take" should "force evaluation of first n elements" in {
    Stream(1, 2, 3, 4, 5, 6, 7).take(2) should be (List(1, 2))
    Stream(1, 2).take(2) should be (List(1, 2))
  }

  "drop" should "drop first n elements" in {
    Stream(1, 2, 3, 4, 5, 6, 7).drop(2).toList should be (List(3, 4, 5, 6, 7))
    Stream(1, 2).drop(2) should be (Empty)
  }

  def correctTakeWhile(takewhile: (Stream[Int], Int => Boolean) => Stream[Int]) = {
    takewhile(Stream(1, 3, 5, 8, 9, 11), (_ % 2 == 1)).toList should be (List(1, 3, 5))
    takewhile(Stream(2, 1, 3), (_ % 2 == 1)) should be (Empty)
    takewhile(Stream[Int](), (_ % 2 == 1)) should be (Empty)

  }

  "takeWhile" should "take all elements that match" in {
    correctTakeWhile((s: Stream[Int], p) => s.takeWhile(p))
  }

  "forAll" should "check if all elements match a predicate" in {
    Stream(1, 3, 5, 11, 9, 9, 7).forAll(_ % 2 == 1) should be (true)
    Stream(1, 2, 5, 11, 9, 9, 7).forAll(_ % 2 == 1) should be (false)
  }

  "takeWhile2" should "take all elements that match" in {
    correctTakeWhile((s: Stream[Int], p) => s.takeWhile2(p))
  }

  "headOption2" should "return head for non-empty list" in {
    Stream(5, 2, 3).headOption2 should be (Some(5))
  }

  it should "return None for empty list" in {
    Empty.headOption2 should be (None)
  }

  "map" should "transform stream elements" in {
    Stream("1", "5", "9").map(_.toInt).toList should be (List(1, 5, 9))
  }

  "filter" should "leave elements that match predicate" in {
    Stream(1, 2, 3, 4, 5, 6).filter(_ % 2 == 0).toList should be (List(2, 4, 6))
  }

  "append" should "put another stream at the end" in {
    Stream(1, 2, 3).append(Stream(4, 5, 6)).toList should be (List(1, 2, 3, 4, 5, 6))
  }

  "flatMap" should "aggregate all streams from transformation of elements" in {
    Stream(1, 2, 3).flatMap(Stream(_)).toList should be (List(1, 2, 3))
  }

  def correctConstant(const: Int => Stream[Int]) = {
    const(6).take(3) should be (List(6, 6, 6))
  }

  "constant" should "provide as long Stream of repeated element as wanted" in {
    correctConstant(constant)
  }

  "constant2" should "provide as long Stream of repeated element as wanted" in {
    correctConstant(constant2)
  }

  def correctFrom(fr: Int => Stream[Int]) = {
    fr(1).take(8) should be (List(1, 2, 3, 4, 5, 6, 7, 8))
  }

  "from" should "provide any number of ascending integers" in {
    correctFrom(from)
  }

  "from2" should "provide any number of ascending integers" in {
    correctFrom(from2)
  }

  def correctFibs(fibz: Stream[Int]) = {
    fibz.take(7) should be (List(0, 1, 1, 2, 3, 5, 8))
  }

  "fibs" should "provide any number of Fibonacci numbers" in {
    correctFibs(fibs)
  }

  "fibs2" should "provide any number of Fibonacci numbers" in {
    correctFibs(fibs2)
  }

  "ones2" should "provide any number of 1s" in {
    ones2.take(3) should be (List(1, 1, 1))
  }

}