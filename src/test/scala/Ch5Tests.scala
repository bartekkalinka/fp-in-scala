/**
 * Created by bka on 2015-02-13.
 */

import org.scalatest.{Matchers, FlatSpec}
import fp.Chapter5._

class Chapter5Spec extends FlatSpec with Matchers {

  "toList" should "force stream evaluation" in {
    Stream(1, 2, 3, 4, 5, 6, 7).toList should be (List(1, 2, 3, 4, 5, 6, 7))
  }
}