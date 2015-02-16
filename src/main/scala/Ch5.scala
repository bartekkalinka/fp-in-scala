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
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    //5.4
    def forAll(p: A => Boolean): Boolean = !exists({x: A => !p(x)})

    //5.5
    def takeWhile2(p: A => Boolean): Stream[A] =
      foldRight(Stream[A]())((a, b) => if(p(a)) Cons(() => a, () => b.takeWhile2(p)) else Empty)

    //5.6
    def headOption2: Option[A] = foldRight[Option[A]](None)((a, b) => Some(a))
 }
  case object Empty extends Stream[Nothing] {
    def headOption: Option[Nothing] = None
    def toList: List[Nothing] = Nil
    def take(n: Int): List[Nothing] = {if(n == 0) Nil else throw new CallForEmptyException}
    def drop(n: Int): Stream[Nothing] = {if(n == 0) this else throw new CallForEmptyException}
    def takeWhile(p: Nothing => Boolean): Stream[Nothing] = this
  }

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
    def headOption: Option[A] = Some(h())
    def toList: List[A] = h() :: t().toList
    def take(n: Int): List[A] =  n match {case 0 => Nil; case m => h() :: t().take(m - 1)}
    def drop(n: Int): Stream[A] = n match {case 0 => this; case m => t().drop(m - 1)}
    def takeWhile(p: A => Boolean): Stream[A] = if(p(h())) Cons(h, () => t().takeWhile(p)) else Empty
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


}