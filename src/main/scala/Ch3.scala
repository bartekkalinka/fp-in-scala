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
class UnequalListsException extends Exception

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

  //3.16
  def add1(l: List[Int]): List[Int] = {
    def add1Acc(l: List[Int], acc: List[Int]): List[Int] = l match {
      case Cons(h, t) => add1Acc(t, Cons(h + 1, acc))
      case Nil => acc
    }
    reverseFold(add1Acc(l, Nil))
  }

  //3.17
  def allToString(l: List[Double]): List[String] = {
    def toStrAcc(l: List[Double], acc: List[String]): List[String] = l match {
      case Cons(h, t) => toStrAcc(t, Cons(h.toString, acc))
      case Nil => acc
    }
    reverseFold(toStrAcc(l, Nil))
  }

  //3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    def mapAcc(l: List[A], acc: List[B]): List[B] = l match {
      case Cons(h, t) => mapAcc(t, Cons(f(h), acc))
      case Nil => acc
    }
    reverseFold(mapAcc(as, Nil))
  }

  //3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    def filterAcc(l: List[A], acc: List[A]): List[A] = l match {
      case Cons(h, t) => if(f(h)) filterAcc(t, Cons(h, acc)) else filterAcc(t, acc)
      case Nil => acc
    }
    reverseFold(filterAcc(as, Nil))
  }

  //3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  //3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as){elem: A => if(f(elem)) List(elem) else Nil}

  //3.22
  def addLists(l1: List[Int], l2: List[Int]) = {
    def addAcc(l1: List[Int], l2: List[Int], acc: List[Int]): List[Int] = (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => addAcc(t1, t2, Cons(h1 + h2, acc))
      case (Nil, Nil) => acc
      case _ => throw new UnequalListsException
    }
    reverseFold(addAcc(l1, l2, Nil))
  }

  //3.23
  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    def zipAcc(l1: List[A], l2: List[B], acc: List[C]): List[C] = (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => zipAcc(t1, t2, Cons(f(h1, h2), acc))
      case (Nil, Nil) => acc
      case _ => throw new UnequalListsException
    }
    reverseFold(zipAcc(l1, l2, Nil))
  }

  //3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(h1, t1), Cons(h2, t2)) => (h1 == h2 && hasSubsequence(t1, t2)) || hasSubsequence(t1, sub)
    case (_, Nil) => true
    case (Nil, _) => false
  }


  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  //3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  //3.26
  def max(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(left, right) => max(left) max max(right)
  }

  //3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => (depth(left) + 1) max (depth(right) + 1)
  }

  //3.28
  def map[A,B](t:Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  //3.29
  def fold[A,B](t:Tree[A])(f: A => B, g: B => B, h: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(left, right) => h(g(fold(left)(f, g, h)), g(fold(right)(f, g, h)))
  }

  def sizeFold[A](t: Tree[A]): Int = fold[A, Int](t)({x => 1}, {x => x}, {(left, right) => left + right})

  def mapFold[A,B](t:Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)({x => Leaf(f(x))}, {x => x}, {(left: Tree[B], right: Tree[B]) => Branch(left, right)} )

  def maxFold(t: Tree[Int]): Int = fold[Int, Int](t)({x => x}, {x => x}, {(left, right) => left max right})

  def depthFold[A](t: Tree[A]): Int = fold[A, Int](t)({x => 0}, {x => x + 1}, {(left, right) => left max right})

}

