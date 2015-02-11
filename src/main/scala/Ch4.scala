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

  //def orElse[B >: A](ob: => Option[B]): Option[B]
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

