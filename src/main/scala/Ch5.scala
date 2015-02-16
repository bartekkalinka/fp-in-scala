/**
 * Created by bka on 2015-02-13.
 */
package fp

object Chapter5 {

  class CallForEmptyException extends Exception

  trait Stream[+A] {
    def headOption: Option[A]

    //5.1
    def toList: List[A]

    //5.2
    def take(n: Int): List[A]

    def drop(n: Int): Stream[A]

    //5.3
    def takeWhile(p: A => Boolean): Stream[A]

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    //5.4
    def forAll(p: A => Boolean): Boolean = !exists({ x: A => !p(x)})

    //5.5
    def takeWhile2(p: A => Boolean): Stream[A] =
      foldRight(Stream[A]())((a, b) => if (p(a)) Cons(() => a, () => b.takeWhile2(p)) else Empty)

    //5.6
    def headOption2: Option[A] = foldRight[Option[A]](None)((a, b) => Some(a))

    //5.7
    def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](Empty)((a, b) => Cons[B](() => f(a), () => b))

    def filter(f: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a, b) => if (f(a)) Cons(() => a, () => b) else b)

    def append[AA >: A](s: => Stream[AA]): Stream[AA] = foldRight[Stream[AA]](s)((a, b) => Cons(() => a, () => b))

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight[Stream[B]](Empty)((a, b) => f(a).append(b))
  }

  case object Empty extends Stream[Nothing] {
    def headOption: Option[Nothing] = None

    def toList: List[Nothing] = Nil

    def take(n: Int): List[Nothing] = {
      if (n == 0) Nil else throw new CallForEmptyException
    }

    def drop(n: Int): Stream[Nothing] = {
      if (n == 0) this else throw new CallForEmptyException
    }

    def takeWhile(p: Nothing => Boolean): Stream[Nothing] = this
  }

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
    def headOption: Option[A] = Some(h())

    def toList: List[A] = h() :: t().toList

    def take(n: Int): List[A] = n match {
      case 0 => Nil;
      case m => h() :: t().take(m - 1)
    }

    def drop(n: Int): Stream[A] = n match {
      case 0 => this;
      case m => t().drop(m - 1)
    }

    def takeWhile(p: A => Boolean): Stream[A] = if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
  }

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  //5.8
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  //5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  //5.10
  private def _fib(a1: Int, a2: Int): Stream[Int] = Stream.cons(a1, _fib(a2, a1 + a2))
  val fibs: Stream[Int] = _fib(0, 1)

  //5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
  }

  //5.12
  //def ones2 = unfold[Int, ()](())({a: () => Some((1, ()))})
}