/**
 * Created by bka on 2015-02-11.
 */
package fp

//4.1
trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map[Option[B]](f).getOrElse(None)

  def filter(f: A => Boolean): Option[A] = if(map[Boolean](f).getOrElse(false)) this else None

  def filter2(f: A => Boolean): Option[A] = flatMap({a: A => if(f(a)) Some(a) else None})

  def orElse[B >: A](ob: => Option[B]): Option[B]
}

case class Some[+A](get: A) extends Option[A] {
  override def orElse[B >: A](ob: => Option[B]): Option[B] = this
}
case object None extends Option[Nothing] {
  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
}

object Chapter4 {

  import Chapter3._

  //4.2
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap({m => mean(xs.map({x => math.pow(x - m, 2)}))})

  //4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _ => None
  }

  //4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    foldLeft[Option[A], Option[List[A]]](a, Some(Nil))({
        case (Some(_acc), Some(k)) => Some(Cons(k, _acc))
        case _ => None
      }).map(reverseFold[A])

  //4.5
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    foldLeft[A, Option[List[B]]](a, Some(Nil))((acc, k) =>
      (acc, f(k)) match {
        case (Some(_acc), Some(l)) => Some(Cons(l, _acc))
        case _ => None
      }
    ).map(reverseFold[B])

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse[Option[A], A](a)({opt => opt})

  //4.6
  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C]
  }

  case class Left[+E](value: E) extends Either[E, Nothing] {
    def map[B](f: Nothing => B): Either[E, B] = Left(value)
    def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = Left(value)
    def orElse[EE >: E,B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C):Either[EE, C] = Left(value)
  }

  case class Right[+A](value: A) extends Either[Nothing, A] {
    def map[B](f: A => B): Either[Nothing, B] = Right(f(value))
    def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)
    def orElse[EE >: Nothing,B >: A](b: => Either[EE, B]): Either[EE, B] = Right(value)
    def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C] = b.map[C]({bb: B => f(value, bb)})
  }

  //4.7
  def traverseE[E, A, B](as: List[A])(
    f: A => Either[E, B]): Either[E, List[B]] = foldLeft[A, Either[E, List[B]]](as, Right(Nil))((acc, k) =>
    (acc, f(k)) match {
      case (Right(_acc), Right(l)) => Right(Cons(l, _acc))
      case (_, Left(e)) => Left(e)
      case (Left(e), _) => Left(e)
    }
  ).map(reverseFold[B])

  def TryE[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def sequenceE[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverseE[E, Either[E, A], A](es)({opt => opt})

  //4.8
  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def map3[E, F, A, B, C](a: Either[E, A], b: Either[E, B], eacc: F)(f: (A, B) => C, g: (F, E) => F):Either[F, C] =
    (a, b) match {
      case (Right(r1), Right(r2)) => Right(f(r1, r2))
      case (Left(e1), Right(r2)) => Left(g(eacc, e1))
      case (Right(r1), Left(e2)) => Left(g(eacc, e2))
      case (Left(e1), Left(e2)) => Left(foldLeft[E, F](List(e2, e1), eacc)(g))
    }

  def mkPerson(name: String, age: Int): Either[List[String], Person] =
    map3(mkName(name), mkAge(age), Nil: List[String])(Person(_, _), {(f, e) => Cons(e, f)})

}

