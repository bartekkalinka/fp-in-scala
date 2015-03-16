/**
 * Created by bka on 12.03.15.
 */

import java.util.concurrent.{TimeoutException, Executors}
import org.scalatest.{Matchers, FlatSpec}
import scala.concurrent.duration._

class Chapter7BlockingSpec extends FlatSpec with Matchers {
  import fp.Chapter7Blocking._
  import fp.Chapter7Blocking.Par._

  def sleepInt(a: Int, duration: Duration): Par[Int] = lazyUnit({Thread.sleep(duration.toMillis); a})

  "map2" should "map correctly" in {
    val es = Executors.newFixedThreadPool(2)
    map2(lazyUnit(1), lazyUnit(2))({(a, b) => (a, b)})(es).get should be ((1, 2))
  }

  it should "work in parallel" in {
    val es = Executors.newFixedThreadPool(4)
    val par = map2(sleepInt(1, Duration(100, "millis")), sleepInt(2, Duration(150, "millis")))({(_, _) => ()})
    noException should be thrownBy par(es).get(180, MILLISECONDS)
    an [TimeoutException] should be thrownBy par(es).get(140, MILLISECONDS)
  }

  "sequence" should "work in parallel" in {
    val es = Executors.newFixedThreadPool(4)
    val list = Range(1, 5).map(i => sleepInt(i, Duration(150, "millis"))).toList
    val par = sequence(list)
    noException should be thrownBy par(es).get(170, MILLISECONDS)
    an [TimeoutException] should be thrownBy par(es).get(140, MILLISECONDS)
  }

  "parFilter" should "filter a list correctly" in {
    val es = Executors.newFixedThreadPool(12)
    val par = parFilter(List(1, 2, 3, 4, 5, 6))({a => a % 2 == 0})
    par(es).get should be (List(2, 4, 6))
  }

  it should "filter a list in parallel" in {
    val es = Executors.newFixedThreadPool(12)
    val par = parFilter(List(1, 2, 3, 4, 5, 6))({a => Thread.sleep(150); a % 2 == 0})
    noException should be thrownBy par(es).get(160, MILLISECONDS)
    an [TimeoutException] should be thrownBy par(es).get(140, MILLISECONDS)
  }

  def correctChoiceN(ch: Par[Int] => List[Par[Int]] => Par[Int]) = {
    val nPar = lazyUnit(3)
    val listPar = List.range(0, 7).map(lazyUnit(_))
    val es = Executors.newFixedThreadPool(12)
    ch(nPar)(listPar)(es).get should be (3)
  }

  "choiceN" should "perform a calculation chosen with result of choice calculation" in {
    correctChoiceN(choiceN[Int])
  }

  "choiceNfMap" should "perform a calculation chosen with result of choice calculation" in {
    correctChoiceN(choiceNfMap[Int])
  }

  "join" should "flatten a Par" in {
    val par = lazyUnit(5)
    val parpar = lazyUnit(map(par)(_ + 2))
    val es = Executors.newFixedThreadPool(2)
    join(parpar)(es).get should be (7)
  }
}

class Chapter7NonblockingSpec extends FlatSpec with Matchers {
  import fp.Chapter7Nonblocking.Par._

  "parMap" should "map a list correctly" in {
    val p = parMap(List.range(1, 10))(math.sqrt(_))
    val x = fp.Chapter7Nonblocking.Par.run(Executors.newFixedThreadPool(2))(p)
    x should be (List(1.0, 1.4142135623730951, 1.7320508075688772, 2.0, 2.23606797749979, 2.449489742783178, 2.6457513110645907, 2.8284271247461903, 3.0))
  }

  it should "handle exceptions" in {
    val p = parMap(List.range(1, 10))({ case 5 => throw new Exception("test"); case i => math.sqrt(i) })
    an [Exception] should be thrownBy fp.Chapter7Nonblocking.Par.run(Executors.newFixedThreadPool(2))(p)
  }

  "fork(lazyunit)" should "not cause a deadlock on 1 thread" in {
    val a = lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(1)
    fp.Chapter7Nonblocking.Par.run(S)(fork(a)) should be (43)
  }

  "parallel calculation throwing an exception" should "result in throwing the exception to the outside" in {
    val p = fork(unit({throw new Exception("test")}))
    an [Exception] should be thrownBy fp.Chapter7Nonblocking.Par.run(Executors.newFixedThreadPool(1))(p)
  }

  "choiceN" should "perform a calculation chosen with result of choice calculation" in {
    val nPar = lazyUnit(3)
    val listPar = List.range(0, 7).map(lazyUnit(_))
    val es = Executors.newFixedThreadPool(12)
    val choicePar = choiceN(nPar)(listPar)
    fp.Chapter7Nonblocking.Par.run(es)(choicePar) should be (3)
  }

  "join" should "flatten a Par" in {
    val par = lazyUnit(5)
    val parpar = lazyUnit(map(par)(_ + 2))
    val es = Executors.newFixedThreadPool(2)
    fp.Chapter7Nonblocking.Par.run(es)(join(parpar)) should be (7)
  }

}


