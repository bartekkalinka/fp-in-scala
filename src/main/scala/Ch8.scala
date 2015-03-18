/**
 * Created by bka on 2015-03-16.
 */

package fp

object Chapter8 {
  import Chapter6.{State, RNG, Rand}

  case class Gen[A](sample: State[RNG,A])

  trait Prop {
    def check: Boolean

    //8.3
    def &&(p: Prop): Prop = new Prop { def check = { check && p.check } }

  }

  object Gen {
    //def forAll[A](a: Gen[A])(f: A => Boolean): Prop

    //8.4
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen[Int](State[RNG, Int]({
        rng: RNG => Chapter6.nonNegativeLessThan(stopExclusive - start)(rng) + start
      }))


  }
}

