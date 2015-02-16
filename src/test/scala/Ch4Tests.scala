/**
 * Created by bka on 2015-02-11.
 */

import java.lang.Exception

import org.scalatest.{Matchers, FlatSpec}
import fp._

class Chapter4Spec extends FlatSpec with Matchers {

  import Chapter3._
  import Chapter4._

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

  "orElse" should "return the first Option if it’s defined" in {
    getIfEven(2).orElse(getIfEven(1)) should be (Some(2))
  }

  it should "it return the second Option otherwise" in {
    getIfEven(1).orElse(getIfEven(2)) should be (Some(2))
    getIfEven(1).orElse(getIfEven(5)) should be (None)
  }

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

  "variance" should "get variance right" in {
    variance(Seq()) should be (None)
    variance(Seq(0.0, 3.0, 6.0)) should be (Some(6.0))
  }

  "map2" should "map 2 options with 1 function" in {
    map2(getIfEven(2), getIfOdd(1))(_ + _) should be (Some(3))
    map2(getIfEven(2), getIfEven(1))(_ + _) should be (None)
    map2(getIfOdd(2), getIfOdd(1))(_ + _) should be (None)
  }

  def correctSequence(seq: List[Option[Int]] => Option[List[Int]]) = {
    seq(List(Some(1), Some(2), Some(3))) should be (Some(List(1, 2, 3)))
    seq(List(Some(1), None, Some(3))) should be (None)
    seq(List()) should be (Some(List()))
  }

  "sequence" should "push option outside of list" in {
    correctSequence(sequence[Int])
  }

  "sequence2" should "push option outside of list" in {
    correctSequence(sequence2[Int])
  }

  "traverse" should "traverse list with function and push option outside of list" in {
    traverse[String, Int](List("1", "2", "3"))({x => Try(x.toInt)}) should be (Some(List(1, 2, 3)))
    traverse[String, Int](List("1", "2", "a"))({x => Try(x.toInt)}) should be (None)
  }

  def evenRight(i: Int) = {if(i % 2 == 0) Right(i) else Left("odd")}
  def oddRight(i: Int) = {if(i % 2 == 1) Right(i) else Left("even")}

  "map" should "transform alternative" in {
    evenRight(2).map {_.toString} should be (Right("2"))
    evenRight(1).map {_.toString} should be (Left("odd"))
  }

  "orElse" should "return first if it's right" in {
    evenRight(2).orElse(evenRight(1)) should be (Right(2))
  }

  it should "it return the second otherwise" in {
    evenRight(1).orElse(evenRight(2)) should be (Right(2))
    evenRight(1).orElse(evenRight(5)) should be (Left("odd"))
  }

  "flatMap" should "transform alternative with function giving alternative result" in {
    evenRight(2).flatMap {evenRight(_)} should be (Right(2))
    evenRight(2).flatMap {oddRight(_)} should be (Left("even"))
    evenRight(1).flatMap {evenRight(_)} should be (Left("odd"))
    oddRight(1).flatMap {oddRight(_)} should be (Right(1))
  }

  "map2" should "map binary operator through alternatives" in {
    evenRight(4).map2(evenRight(2))(_ + _) should be (Right(6))
    evenRight(3).map2(evenRight(2))(_ + _) should be (Left("odd"))
    oddRight(3).map2(oddRight(2))(_ + _) should be (Left("even"))
    oddRight(3).map2(oddRight(5))(_ + _) should be (Right(8))
  }

  "sequenceE" should "push alternative outside of list" in {
    sequenceE(List(Right(1), Right(2), Right(3))) should be (Right(List(1, 2, 3)))
    sequenceE(List(Right(1), Left("none"), Right(3))) should be (Left("none"))
    sequenceE(List()) should be (Right(List()))
  }

  "traverseE" should "traverse list with function and push alternative outside of list" in {
    traverseE[Exception, String, Int](List("1", "2", "3"))({x => TryE(x.toInt)}) should be (Right(List(1, 2, 3)))
    traverseE[Exception, String, Int](List("1", "2", "a"))({x => TryE(x.toInt)}) should matchPattern { case Left(_) => }
  }

  "mkPerson" should "list all exceptions" in {
    mkPerson("", -5) should be (Left(List("Name is empty.", "Age is out of range.")))
  }

}