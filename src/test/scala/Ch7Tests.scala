/**
 * Created by bka on 12.03.15.
 */

import java.util.concurrent.{TimeoutException, Executors}

import org.scalatest.{Matchers, FlatSpec}
import fp.Chapter7._
import fp.Chapter7.Par._

import scala.concurrent.duration._

class Chapter7Spec extends FlatSpec with Matchers {
  def sleepInt(a: Int, duration: Duration): Par[Int] = lazyUnit({Thread.sleep(duration.toMillis); a})

  "map2" should "map correctly" in {
    val es = Executors.newFixedThreadPool(2)
    map2(lazyUnit(1), lazyUnit(2))({(a, b) => (a, b)})(es).get should be ((1, 2))
  }

  it should "work in parallel" in {
    val es = Executors.newFixedThreadPool(4)
    val par = map2(sleepInt(1, Duration(100, "millis")), sleepInt(2, Duration(150, "millis")))({(_, _) => ()})
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
}


