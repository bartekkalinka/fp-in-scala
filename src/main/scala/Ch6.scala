/**
 * Created by bka on 2015-02-25.
 */

package fp

object Chapter6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  //6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    val nonNegative = n match {
      case Int.MinValue => 0
      case i => Math.abs(i)
    }
    (nonNegative, nextRNG)
  }

  //6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue.toDouble, nextRNG)
  }

  //6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, nextRNG) = rng.nextInt
    val (d, nextRNG2) = double(nextRNG)
    ((n, d), nextRNG2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, nextRNG) = double(rng)
    val (n, nextRNG2) = nextRNG.nextInt
    ((d, n), nextRNG2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, nextRNG) = double(rng)
    val (d2, nextRNG2) = double(nextRNG)
    val (d3, nextRNG3) = double(nextRNG2)
    ((d1, d2, d3), nextRNG3)
  }

  //6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (Nil, rng)
    case cnt => {
      val (n, nextRNG) = rng.nextInt
      val (tail, finalRNG) = ints(count - 1)(nextRNG)
      (n :: tail, finalRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  //6.5
  def double2: Rand[Double] = map[Int, Double](nonNegativeInt)(n => n.toDouble / Int.MaxValue.toDouble)

  //6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  //6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil => unit(Nil)
    case h :: t => map2(h, sequence(t))((a, b) => a :: b)
  }

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(_.nextInt))

  //6.8
  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (i, rng2) = f(rng)
    g(i)(rng2)
  }

  def nonNegativeLessThan2(n: Int): Rand[Int] = flatMap(_.nextInt)({
    i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        (mod, _)
      else nonNegativeLessThan2(n)
  })

  //6.9
  def mapFlat[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap[A, B](s)(a => (rng => (f(a), rng)))

  def map2Flat[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap[(A, B), C](rng => {val (a, rngb) = ra(rng); val (b, rngc) = rb(rngb); ((a, b), rngc)})({ case (a, b) => (rng => (f(a, b), rng))})

  //6.10
  case class State[S,+A](run: S => (A,S)) {
    def flatMap[B](g: A => State[S, B]): State[S, B] =
      State {s: S =>
        val (a, s2) = run(s)
        g(a).run(s2)
      }

    def map[B](f: A => B): State[S, B] = flatMap[B](a => State(s => (f(a), s)))

    def map2[B,C](rb: State[S, B])(f: (A, B) => C): State[S, C] = State {
        s =>
        val (a, s2) = run(s)
        val (b, s3) = rb.run(s2)
        (f(a, b), s3)
      }
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State {s: S => (a, s)}

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs match {
      case Nil => unit(Nil)
      case h :: t => h.map2(sequence(t))((a, b) => a :: b)
    }
  }

  //6.11
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def machineStep(input: Input, inpMach: Machine): Machine = input match {
      case Turn => if(inpMach.locked) inpMach else Machine(true, inpMach.candies - 1, inpMach.coins)
      case Coin => if(inpMach.locked && inpMach.candies > 0) Machine(false, inpMach.candies, inpMach.coins + 1) else inpMach
    }
    def step(input: Input): State[Machine, (Int, Int)] =
      State { inpMach => val m = machineStep(input, inpMach); ((m.candies, m.coins), m) }
    State.sequence(inputs.map(step(_))).map(_.last)
  }

}