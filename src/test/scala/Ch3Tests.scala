/**
 * Created by bka on 2015-02-10.
 */
import org.scalatest.{Matchers, FlatSpec}
import fp._

class Chapter3Spec extends FlatSpec with Matchers {
  "val x" should "equal correct value" in {
    Chapter3.x should be (3)
  }

  "tail" should "throw exception for Nil" in {
    a [CallForNilException] should be thrownBy {
      Chapter3.tail(Nil)
    }
  }

  it should "return tail for non-empty List" in {
    Chapter3.tail(List(5, 3, 4, 2)) should be (List(3, 4, 2))
  }

  "setHead" should "throw exception for Nil" in {
    a [CallForNilException] should be thrownBy {
      Chapter3.setHead(Nil, 1)
    }
  }

  it should "set head for non-empty List" in {
    Chapter3.setHead(List(5), 1) should be (List(1))
    Chapter3.setHead(List(5, 3, 4, 2), 1) should be (List(1, 3, 4, 2))
  }

  "drop" should "throw exception for Nil" in {
    a [CallForNilException] should be thrownBy {
      Chapter3.drop(Nil, 1)
    }
  }

  it should "return unchanged list when dropping zero elements" in {
    Chapter3.drop(Nil, 0) should be (Nil)
    Chapter3.drop(List(5, 2, 9), 0) should be (List(5, 2, 9))
  }

  it should "drop elements correctly" in {
    Chapter3.drop(List(5, 2, 9), 3) should be (Nil)
    Chapter3.drop(List(2, 5, 1, 9, 2), 2) should be (List( 1, 9, 2))
  }

}

