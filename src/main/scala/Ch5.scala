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