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
  //TODO
  //def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]]

}

