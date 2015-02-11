/**
 * Created by bka on 2015-02-11.
 */
import org.scalatest.{Matchers, FlatSpec}
import fp._

class Chapter4Spec extends FlatSpec with Matchers {

  //import Chapter4._

  def getIfEven(i: Int) = {if(i % 2 == 0) Some(i) else None}

  "map" should "transform optional element" in {
    getIfEven(2).map {_.toString} should be (Some("2"))
    getIfEven(1).map {_.toString} should be (None)
  }

}