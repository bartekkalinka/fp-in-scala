import org.scalatest.{Matchers, FlatSpec}
import fp.Chapter2

/**
 * Created by bka on 2015-02-10.
 */
class Chapter2Spec extends FlatSpec with Matchers {
  "fib" should "calculate fibonacci correctly" in {
    Chapter2.fib(2) should be (2)
    Chapter2.fib(3) should be (3)
    Chapter2.fib(4) should be (5)
    Chapter2.fib(5) should be (8)
    Chapter2.fib(6) should be (13)
    Chapter2.fib(7) should be (21)
  }

  "isSorted" should "check for integer array ordering correctly" in {
    Chapter2.isSorted[Int](Array(2, 5, 6, 7, 3), (_ < _)) should be (false)
    Chapter2.isSorted[Int](Array(2, 5, 6, 7, 8), (_ < _)) should be (true)
  }

  it should "check for string array ordering correctly" in {
    Chapter2.isSorted[String](Array("ac", "ad", "ba", "yu", "ya"), (_ < _)) should be (false)
    Chapter2.isSorted[String](Array("ac", "ad", "ba", "yu", "yz"), (_ < _)) should be (true)
  }


  "curry" should "curry function correctly" in {
    val f = Chapter2.curry[Int, Int, Int](_ + _)(5)
    f(3) should be (8)
    f(6) should be (11)
  }

  "uncurry" should "uncurry function correctly" in {
    val f = Chapter2.uncurry({x: Int => {y: Int => x + y}})
    f(5, 3) should be (8)
    f(5, 6) should be (11)
  }

  "compose" should "compose 2 functions correctly" in {
    def f(x: Int) = x + 1
    def g(x: Int) = x - 1
    val h = Chapter2.compose(f, g)
    h(5) should be (5)
    h(9) should be (9)
  }
}



