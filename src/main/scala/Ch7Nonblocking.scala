package fp

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.AtomicReference

import fp.parallelism.Actor

/**
 * Created by bka on 14.03.15.
 */

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

    //TODO DRY with respect to blocking implementation
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def map[A,B](a: Par[A])(f: A => B): Par[B] = map2[A, A, B](a, a)((a, b) => f(a))

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
      case h :: t => map2[A, List[A], List[A]](h, sequence[A](t))(_ :: _)
      case List(p) => map(p)(List(_))
      case Nil => unit(Nil)
    }

    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }
  }

  //for console tests
  object TestPar {
    import Par._

    //fp.Chapter7Nonblocking.TestPar.test1
    def test1 = {
      val p = parMap(List.range(1, 10))(math.sqrt(_))
      val x = run(Executors.newFixedThreadPool(2))(p)
      println(x)
    }

    //test that there's no deadlock as with blocking implementation
    //fp.Chapter7Nonblocking.TestPar.noDeadlockDemo1
    def noDeadlockDemo1 = {
      val a = lazyUnit(42 + 1)
      val S = Executors.newFixedThreadPool(1)
      println(run(S)(fork(a)))
    }

    //test of exception handling
    //fp.Chapter7Nonblocking.TestPar.testException
    def testException = {
      val p = fork(unit({throw new Exception("test")}))
      val x = run(Executors.newFixedThreadPool(1))(p)
    }

    //to better understand problem with exception handling
    //fp.Chapter7Nonblocking.TestPar.testException2
    def testException2 = {
      val es = Executors.newFixedThreadPool(1)
      es.submit(new Callable[Unit] { def call = {throw new Exception("test")} })
    }
  }
}