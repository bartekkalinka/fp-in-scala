/**
 * Created by bka on 07.03.15.
 */
package fp

import scala.concurrent.duration._
import java.util.concurrent._

object Chapter7 {

  type Par[A] = ExecutorService => Future[A]

  object Par {

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    //7.3
    private case class Map2Future[A,B,C](af: Future[A], bf: Future[B], f: (A, B) => C) extends Future[C] {
      def isDone = af.isDone && bf.isDone
      def get = {
        f(af.get, bf.get)
      }
      def get(timeout: Long, units: TimeUnit) = {
        val start = System.currentTimeMillis()
        val a = af.get(timeout, units)
        val afDuration = Duration(System.currentTimeMillis - start, MILLISECONDS)
        val bfDurationAllowed = Duration(timeout, units) - afDuration
        val b = bf.get(bfDurationAllowed.toMillis, MILLISECONDS)
        f(a, b)
      }
      def isCancelled = af.isCancelled || bf.isCancelled
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
      (es: ExecutorService) => Map2Future(a(es), b(es), f)

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    //7.4
    def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    //7.5
    def map[A,B](a: Par[A])(f: A => B): Par[B] = map2[A, A, B](a, a)((a, b) => f(a))

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
      case h :: t => map2[A, List[A], List[A]](h, sequence[A](t))(_ :: _)
      case List(p) => map(p)(List(_))
      case Nil => unit(Nil)
    }

    //7.6
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
      map(
        sequence(
          as.map(a => lazyUnit(
            if(f(a)) Some(a) else None)
          )
        )
      )(_.flatten)

  }

  object TestPar {
    import Par._

    def sleepPrintPar(a: String, duration: Duration): Par[Unit] = lazyUnit({Thread.sleep(duration.toMillis); println(a)})

    //TODO move to scalatest
    //sequence parallelism test
    //Chapter7.TestPar.test1(4)
    def test1(threads: Integer) = {
      val es = Executors.newFixedThreadPool(threads)
      val list = Range(1, 5).map(i => sleepPrintPar(i.toString, Duration((5 - i) * 100 + 4000, "millis"))).toList
      sequence(list)(es)
    }

  }

}