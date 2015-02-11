package fp

object Chapter2 {
  //2.1
  def fib(n: Int): Int = {
    def fibAcc(k: Int, f1: Int, f2: Int): Int = {
      if(k < n) fibAcc(k + 1, f1 + f2, f1) else f2
    }
    fibAcc(0, 1, 0)
  }

  //2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(pos: Int): Boolean = {
      if(pos < as.length - 1)
        ordered(as(pos), as(pos + 1)) && loop(pos + 1)
      else
        true
    }
    loop(0)
  }

  //2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {a: A => {b: B => f(a, b)}}

  //2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {(a: A, b: B) => f(a)(b)}

  //2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = {a: A => f(g(a))}
}