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

  "takeWhile" should "take all elements that match" in {
    Stream(1, 3, 5, 8, 9, 11).takeWhile(_ % 2 == 1).toList should be (List(1, 3, 5))
    Stream(2, 1, 3).takeWhile(_ % 2 == 1) should be (Empty)
    Stream[Int]().takeWhile(_ % 2 == 1) should be (Empty)
  }

}