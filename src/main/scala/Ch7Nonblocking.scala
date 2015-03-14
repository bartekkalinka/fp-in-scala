package fp

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.AtomicReference

import fp.parallelism.Actor

/**
 * Created by bka on 14.03.15.
 */

object Chapter7Nonblocking {
  sealed trait Future[+A] {
    private[fp] def apply(k: Either[Exception, A] => Unit): Unit
  }
  type Par[+A] = ExecutorService => Future[A]

  object Par {
    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[Either[Exception, A]]
      val latch = new CountDownLatch(1)
      p(es) { ea => ref.set(ea); latch.countDown }
      latch.await
      ref.get match {
        case Right(a) => a
        case Left(e) => throw e
      }
    }

    def unit[A](a: => A): Par[A] =
      es => new Future[A] {
        def apply(cb: Either[Exception, A] => Unit): Unit = {
          try {
            cb(Right(a))
          }
          catch {
            case e: Exception => cb(Left(e))
          }
        }
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: Either[Exception, A] => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: Either[Exception, C] => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          val combiner = Actor[Either[Exception,Either[A,B]]](es) {
            case Right(Left(a)) =>
              if (br.isDefined) eval(es)(cb(Right(f(a,br.get))))
              else ar = Some(a)
            case Right(Right(b)) =>
              if (ar.isDefined) eval(es)(cb(Right(f(ar.get,b))))
              else br = Some(b)
            case Left(e) => eval(es)(cb(Left(e)))
          }
          p(es)({case Right(a) => combiner ! Right(Left(a)); case Left(e) => combiner ! Left(e)})
          p2(es)({case Right(b) => combiner ! Right(Right(b)); case Left(e) => combiner ! Left(e)})
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

    //to better understand problem with exception handling
    //fp.Chapter7Nonblocking.TestPar.testException2
    def testException2 = {
      val es = Executors.newFixedThreadPool(1)
      es.submit(new Callable[Unit] { def call = {throw new Exception("test")} })
    }
  }
}