/**
 * Created by bka on 07.03.15.
 */
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

    def sleepInt(a: Int, duration: Duration): Par[Int] = lazyUnit({Thread.sleep(duration.toMillis); a})

    def sleepPrintPar(a: String, duration: Duration): Par[Unit] = lazyUnit({Thread.sleep(duration.toMillis); println(a)})

    //showing parallel execution:
    //for threads >= 2, output is: 1. bbb, 2. aaa
    def test1(threads: Integer) = {
      val es = Executors.newFixedThreadPool(threads)
      val f1 = sleepPrintPar("aaa", Duration(5000, "millis"))(es)
      val f2 = sleepPrintPar("bbb", Duration(1000, "millis"))(es)
    }

    //showing map2 parallel execution:
    //for threads >= 2, output is: 1. bbb, 2. aaa
    def test2(threads: Integer) = {
      val es = Executors.newFixedThreadPool(threads)
      val par = map2(sleepPrintPar("aaa", Duration(5000, "millis")), sleepPrintPar("bbb", Duration(1000, "millis")))({(_, _) => ()})
      val f = par(es)
    }

    //testing timeout on map2
    //Chapter7.TestPar.test3(2, 1500L) ok
    //Chapter7.TestPar.test3(2, 1400L) TimeoutException
    def test3(threads: Integer, timeoutMillis: Long) = {
      val es = Executors.newFixedThreadPool(threads)
      val par = map2(sleepPrintPar("aaa", Duration(1000, "millis")), sleepPrintPar("bbb", Duration(1500, "millis")))({(_, _) => ()})
      par(es).get(timeoutMillis, MILLISECONDS)
    }

    //sequence parallelism test
    //Chapter7.TestPar.test4(4)
    def test4(threads: Integer) = {
      val es = Executors.newFixedThreadPool(threads)
      val list = Range(1, 5).map(i => sleepPrintPar(i.toString, Duration((5 - i) * 100 + 4000, "millis"))).toList
      sequence(list)(es)
    }

    //parFilter test of correctness + parallelism
    def test5 = {
      val es = Executors.newFixedThreadPool(12)
      val par = parFilter(List(1, 2, 3, 4, 5, 6))({a => Thread.sleep(1500); a % 2 == 0})
      val start = System.currentTimeMillis()
      println(par(es).get(1600, MILLISECONDS))
      println(Duration(System.currentTimeMillis - start, MILLISECONDS))
    }

  }

}