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

  def reverse[A](l: List[A]) = {
    def revAcc(k: List[A], acc: List[A]): List[A] = {
      k match {
        case Nil => acc
        case Cons(h, t) => revAcc(t, Cons(h, acc))
      }
    }
    revAcc(l, Nil)
  }

  //3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    def dropAcc(m: List[A], acc: List[A]): List[A] = {
      m match {
        case Nil => acc
        case Cons(h, t) => dropAcc(t, if (f(h)) acc else Cons(h, acc))
      }
    }
    reverse(dropAcc(l, Nil))
  }

  //3.6
  def init[A](l: List[A]): List[A] = {
    def initAcc(k: List[A], acc: List[A]): List[A] = {
      k match {
        case Nil => throw new CallForNilException
        case Cons(h, Nil) => acc
        case Cons(h, t) => initAcc(t, Cons(h, acc))
      }
    }
    reverse(initAcc(l, Nil))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  //3.7
  def foldRightWStopper[A,B](as: List[A], z: B, stopper: A => Boolean)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => if(stopper(x)) f(x, z) else f(x, foldRight(xs, z)(f))
    }

  def product3(ns: List[Double]) = foldRightWStopper(ns, 1.0, {x: Double => x == 0.0})(_ * _)

  //3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((x, y) => y + 1)

  //3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  //3.11
  def productLeft(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def sumLeft(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + y)
  def lengthLeft[A](as: List[A]): Int = foldRight(as, 0)((x, y) => y + 1)

  //3.12
  def reverseFold[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc: List[A], elem: A) => Cons(elem, acc))

  //3.13
  def foldRightL[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    val rev = foldLeft(as, Nil: List[A])((acc: List[A], elem: A) => Cons(elem, acc))
    foldLeft[A, B](rev ,z){(b: B, a: A) => f(a, b)}
  }

  //3.14
  def append[A](l1: List[A], l2: List[A]) = {
    val rev1 = reverseFold(l1)
    val revSum = foldLeft(l2, rev1)((acc: List[A], elem: A) => Cons(elem, acc))
    reverseFold(revSum)
  }

  //3.15
  def flatten[A](l: List[List[A]]): List[A] = {
    reverseFold(
      foldLeft(l, Nil: List[A])
      {
        (sum, elemList) =>
          foldLeft(elemList, sum)((acc: List[A], elem: A) => Cons(elem, acc))
      }
    )
  }

}

