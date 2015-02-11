/**
 * Created by bka on 2015-02-11.
 */
import org.scalatest.{Matchers, FlatSpec}
import fp._

class Chapter4Spec extends FlatSpec with Matchers {

  //import Chapter4._

  def getIfEven(i: Int) = {if(i % 2 == 0) Some(i) else None}
  def getIfOdd(i: Int) = {if(i % 2 == 1) Some(i) else None}

  "map" should "transform optional element" in {
    getIfEven(2).map {_.toString} should be (Some("2"))
    getIfEven(1).map {_.toString} should be (None)
  }
  "getOrElse" should "return the result inside the Some case of the Option" in {
    getIfEven(2).getOrElse(5) should be (2)
  }

  it should "return the given default value if the Option is None" in {
    getIfEven(1).getOrElse(5) should be (5)
  }

/*
  "orElse" should "return the first Option if it’s defined" in {
    getIfEven(2).orElse(getIfEven(1)) should be (Some(2))
  }

  it should "it return the second Option otherwise" in {
    getIfEven(1).orElse(getIfEven(2)) should be (Some(2))
    getIfEven(1).orElse(getIfEven(5)) should be (None)
  }
*/

  "flatMap" should "transform optional element with function giving optional result" in {
    getIfEven(2).flatMap {getIfEven(_)} should be (Some(2))
    getIfEven(2).flatMap {getIfOdd(_)} should be (None)
    getIfEven(1).flatMap {getIfOdd(_)} should be (None)
    getIfOdd(1).flatMap {getIfOdd(_)} should be (Some(1))
  }

  "filter" should "express logic value in terms of option" in {
    getIfEven(2).filter(_ % 2 == 0) should be (Some(2))
    getIfEven(1).filter(_ % 2 == 0) should be (None)
    getIfOdd(1).filter(_ % 2 == 0) should be (None)
    getIfOdd(1).filter(_ % 2 == 1) should be (Some(1))
  }

  "filter2" should "express logic value in terms of option" in {
    getIfEven(2).filter2(_ % 2 == 0) should be (Some(2))
    getIfEven(1).filter2(_ % 2 == 0) should be (None)
    getIfOdd(1).filter2(_ % 2 == 0) should be (None)
    getIfOdd(1).filter2(_ % 2 == 1) should be (Some(1))
  }

}