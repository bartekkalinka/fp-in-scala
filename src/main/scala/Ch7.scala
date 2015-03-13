/**
 * Created by bka on 07.03.15.
 */
package fp

import java.util.concurrent.atomic.AtomicReference
import scala.collection.parallel.immutable.ParMap
import scala.concurrent.duration._
import java.util.concurrent._
import fp.parallelism.Actor

object Chapter7Blocking {

  type Par[A] = ExecutorService => Future[A]

  object Par {

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      p(e).get == p2(e).get

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

  //for console tests
  object TestPar {
    import Par._

    def sleepPrintPar(a: String, duration: Duration): Par[Unit] = lazyUnit({Thread.sleep(duration.toMillis); println(a)})

    //fp.Chapter7Blocking.TestPar.deadlockDemo1(2) returns true
    //fp.Chapter7Blocking.TestPar.deadlockDemo1(1) hangs
    def deadlockDemo1(threads: Int) = {
      val a = lazyUnit(42 + 1)
      val S = Executors.newFixedThreadPool(threads)
      println(Par.equal(S)(a, fork(a)))
    }

    //7.9
    //fp.Chapter7Blocking.TestPar.deadlockDemo2(4) hangs
    def deadlockDemo2(threads: Int) = {
      val a = lazyUnit(42 + 1)
      val fa = Range(1, threads + 1).foldLeft(a)((b, i) => fork(b))
      val es = Executors.newFixedThreadPool(threads)
      println(fa(es).get)
    }

  }

}

object Chapter7Nonblocking {
  sealed trait Future[+A] {
    private[fp] def apply(k: A => Unit): Unit
  }
  type Par[+A] = ExecutorService => Future[A]

  object Par {
    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a => ref.set(a); latch.countDown}
      latch.await
      ref.get
    }

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          val combiner = Actor[Either[A,B]](es) {
            case Left(a) =>
              if (br.isDefined) eval(es)(cb(f(a,br.get)))
              else ar = Some(a)
            case Right(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
              else br = Some(b)
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    //TODO does not compile
    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }
  }

  //for console tests
  object TestPar {
    import Par._

    def test1 = {
      import java.util.concurrent.Executors
      val p = parMap(List.range(1, 100000))(math.sqrt(_))
      val x = run(Executors.newFixedThreadPool(2))(p)
    }
  }
}