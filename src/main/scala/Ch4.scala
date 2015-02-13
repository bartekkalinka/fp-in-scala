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

}

