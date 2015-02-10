/**
 * Created by bka on 2015-02-10.
 */
package fp

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

class CallForNilException extends Exception

object Chapter3 {
  //3.1
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  //3.2
  def tail[A](ds: List[A]) = ds match {
    case Nil => throw new CallForNilException
    case Cons(_, t) => t
  }

  //3.3
  def setHead[A](ds: List[A], nh: A) = ds match {
    case Nil => throw new CallForNilException
    case Cons(h, t) => Cons(nh, t)
  }

  //3.4
  def drop[A](l: List[A], n: Int): List[A]  = {
    (l, n) match {
      case (l, 0) => l
      case(Nil, n) => throw new CallForNilException
      case(Cons(h, t), n) => drop(t, n - 1)
    }
  }

}

