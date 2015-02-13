/**
 * Created by bka on 2015-02-13.
 */
package fp

object Chapter5 {
  trait Stream[+A] {
    def headOption: Option[A]

    //5.1
    def toList: List[A]
 }
  case object Empty extends Stream[Nothing] {
    def headOption: Option[Nothing] = None
    def toList: List[Nothing] = Nil
  }
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
    def headOption: Option[A] = Some(h())
    def toList: List[A] = h() :: t().toList
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