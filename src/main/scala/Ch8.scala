/**
 * Created by bka on 2015-03-16.
 */

package fp

object Chapter8 {
  import Chapter6.{State, RNG}

  case class Gen[A](sample: State[RNG,A])

  trait Prop {
    def check: Boolean

    //8.3
    def &&(p: Prop): Prop = new Prop { def check = { check && p.check } }

  }

  object Gen {
    //def forAll[A](a: Gen[A])(f: A => Boolean): Prop

    //8.4

    //cheat sheet:
    //type Rand[+A] = RNG => (A, RNG)
    //def nonNegativeLessThan(n: Int): Rand[Int]
    //Rand[Int] is (RNG => (Int, RNG))
    //case class State[S,+A](run: S => (A,S))

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen[Int](State[RNG, Int]({
        rng: RNG => {
          val (num, rngAfter) = Chapter6.nonNegativeLessThan(stopExclusive - start)(rng)
          (num + start, rngAfter)
        }
      }))

    //8.5
    def unit[A](a: => A): Gen[A] = Gen(State((a, _)))

    def boolean: Gen[Boolean] = Gen(State({
      rng =>
        val (num, rng2) = choose(1, 11).sample.run(rng)
        (num % 2 == 0, rng2)
    }))

    //def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]]
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(
      State.sequence[RNG, A](List.fill[State[RNG, A]](n)(g.sample))
    )
  }

}

