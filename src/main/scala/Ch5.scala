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

    //5.13
    //def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]
    def mapUnfold[B](f: A => B): Stream[B] = unfold[B, Stream[A]](this)({case Cons(h, t) => Some((f(h()), t())); case _ => None})

    def takeUnfold(n: Int): List[A] =
      unfold[A, (Stream[A], Int)]((this, n))({
        case (Cons(h, t), k) => if(k > 0) Some(h(), (t(), k - 1)) else None
        case _ => None
      }).toList

    def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold[A, Stream[A]](this)({
      case Cons(h, t) => if(p(h())) Some(h(), t()) else None
      case _ => None
    })

    def zipWith[B,C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold[C, (Stream[A], Stream[B])](this, s2)({
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    })

    def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold[(Option[A],Option[B]), (Stream[A], Stream[B])](this, s2)({
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    })

    //5.14
    def startsWith[A](s: Stream[A]): Boolean = zipAll(s).forAll({case (Some(a), Some(b)) => a == b; case (Some(_), None) => true; case _ => false})

    //5.15
    def tails: Stream[Stream[A]] = unfold[Stream[A], Stream[A]](this)({
      case Cons(h, t) => Some(Cons(h, t), t())
      case _ => None
    })

    //5.16
    def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = foldRight[Stream[B]](Empty)({
      case (a, Empty) => Stream(f(a, z), z)
      case (a, Cons(h, t)) => Stream.cons[B](f(a, h()), Cons(h, t))

    })
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
  def ones2 = unfold[Int, Unit](())({case () => Some((1, ()))})

  def constant2[A](a: A): Stream[A] = unfold[A, Unit](())({case () => Some((a, ()))})

  def from2(n: Int): Stream[Int] = unfold[Int, Int](n)(a => Some(a, a + 1))

  val fibs2: Stream[Int] = unfold[Int, (Int, Int)]((0, 1)){case (n1, n2) => Some((n1, (n2, n1 + n2)))}


}
