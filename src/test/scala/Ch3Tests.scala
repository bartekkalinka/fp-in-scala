/**
 * Created by bka on 2015-02-10.
 */
import org.scalatest.{Matchers, FlatSpec}
import fp._

class Chapter3Spec extends FlatSpec with Matchers {
  import Chapter3._

  "val x" should "equal correct value" in {
    x should be (3)
  }

  "tail" should "throw exception for Nil" in {
    a [CallForNilException] should be thrownBy {
      tail(Nil)
    }
  }

  it should "return tail for non-empty List" in {
    tail(List(5, 3, 4, 2)) should be (List(3, 4, 2))
  }

  "setHead" should "throw exception for Nil" in {
    a [CallForNilException] should be thrownBy {
      setHead(Nil, 1)
    }
  }

  it should "set head for non-empty List" in {
    setHead(List(5), 1) should be (List(1))
    setHead(List(5, 3, 4, 2), 1) should be (List(1, 3, 4, 2))
  }

  "drop" should "throw exception for Nil" in {
    a [CallForNilException] should be thrownBy {
      drop(Nil, 1)
    }
  }

  it should "return unchanged list when dropping zero elements" in {
    drop(Nil, 0) should be (Nil)
    drop(List(5, 2, 9), 0) should be (List(5, 2, 9))
  }

  it should "drop elements correctly" in {
    drop(List(5, 2, 9), 3) should be (Nil)
    drop(List(2, 5, 1, 9, 2), 2) should be (List( 1, 9, 2))
  }

  "dropWhile" should "leave Nil unchanged" in {
    dropWhile(Nil, {x: Any => true}) should be (Nil)
    dropWhile(Nil, {x: Any => false}) should be (Nil)
  }

  it should "drop filtered elements correctly" in {
    dropWhile(List(5, 2, 3, 7, 2), {x: Any => true}) should be (Nil)
    dropWhile(List(5, 2, 3, 7, 2), {x: Any => false}) should be (List(5, 2, 3, 7, 2))
    def even(x: Int) = { x % 2 == 0 }
    dropWhile(List(5, 2, 3, 7, 2), even) should be (List(5, 3, 7))
  }

  "init" should "throw exception for Nil" in {
    a[CallForNilException] should be thrownBy {
      init(Nil)
    }
  }

  it should "return initial list correctly" in {
    init(List(5)) should be (Nil)
    init(List(5, 4, 3, 6)) should be (List(5, 4, 3))
  }

  def correctProduct(prod: List[Double] => Double) = {
    prod(List(1.0, 2.0, 4.0)) should be (8.0)
    prod(List(1.0, 2.0, 4.0, 0.0, 10242342.0, 23847928.0, 293872938.0, 29387423.0, 28723984.0)) should be (0.0)
  }

  "product3" should "multiply correctly" in {
    correctProduct(product3)
  }

  //3.8
  "foldRight" should "rebuild a list" in {
    foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) should be (List(1,2,3))
  }

  def correctLength(len: List[Int] => Int) = {
    len(Nil) should be (0)
    len(List(6)) should be (1)
    len(List(2, 3, 4, 6, 8, 23, 3, 2)) should be (8)
  }

  "length" should "calculate length correctly" in {
    correctLength(Chapter3.length)
  }

  "foldLeft" should "fold correctly" in {
    foldLeft(List(2, 3, 4), 1)(_ * _) should be (24)
    foldLeft(List("a", "b", "c"), "")(_ + _) should be ("abc")
  }

  "productLeft" should "multiply correctly" in {
    correctProduct(productLeft)
  }

  "sumLeft" should "sum correctly" in {
    sumLeft(List(5, 1, 66)) should be (72)
    sumLeft(Nil) should be (0)
  }

  "lengthLeft" should "calculate length correctly" in {
    correctLength(lengthLeft)
  }

  "reverseFold" should "reverse correctly" in {
    reverseFold(List(1, 2, 3, 4)) should be (List(4, 3, 2, 1))
    reverseFold(Nil) should be (Nil)
  }

  "foldRightL" should "fold a list from right to left" in {
    foldRightL(List(1, 2, 3), (1.0, 0.0)){(elem, acc) => acc match {case (m, s) => (m * 0.1, s + m * elem)}}._2 should be (3.21)
  }

  "append" should "append list 2 to list 1" in {
    append(List(4, 3, 4), List(1, 5, 6)) should be (List(4, 3, 4, 1, 5, 6))
  }

  "flatten" should "make one appended list out of all element lists" in {
    flatten(List(List(1, 2, 3), List(2, 3, 4), Nil, List(7, 8, 9))) should be (List(1, 2, 3, 2, 3, 4, 7, 8, 9))
  }

  "add1" should "add 1 to each element" in {
    add1(List(1, 2, 3, 4)) should be (List(2, 3, 4, 5))
  }

  "allToString" should "convert all doubles to strings" in {
    allToString(List(1.2, 6.2, 4.63, 4.0)) should be (List("1.2", "6.2", "4.63", "4.0"))
  }

  "map" should "map list elements with a function" in {
    map(List(1, 2, 3))(_ + 1) should be (List(2, 3, 4))
  }

  def correctFilter(filt: List[Int] => (Int => Boolean) => List[Int]) = {
    filt(List(1, 2, 3, 4, 5, 6)){_ % 2 == 0} should be (List(2, 4, 6))
  }

  "filter" should "leave out list elements that don't match the predicate" in {
    correctFilter(filter)
  }

  "flatMap" should "flatten the map result" in {
    flatMap(List(1,2,3))(i => List(i,i)) should be (List(1,1,2,2,3,3))
  }

  "filter2" should "leave out list elements that don't match the predicate" in {
    correctFilter(filter2)
  }

  "addLists" should "add all elements of both lists together" in {
    addLists(List(1, 2, 3), List(5, 4, 5)) should be (List(6, 6, 8))
  }

  "zipWith" should "act like addLists in one specific case" in {
    zipWith(List(1, 2, 3), List(5, 4, 5))(_ + _) should be (List(6, 6, 8))
  }

  "hasSubsequence" should "test for subsequences correctly" in {
    hasSubsequence(List(1, 2, 3, 4), List(2, 3)) should be (true)
    hasSubsequence(List(1, 2, 3, 4), List(4, 3)) should be (false)
    hasSubsequence(List(1, 2), List(2, 3, 4)) should be (false)
    hasSubsequence(List(1, 2), Nil) should be (true)
    hasSubsequence(Nil, Nil) should be (true)
  }

  val testTree = Branch(
    Leaf(2),
    Branch(
      Branch(
        Leaf(1),
        Leaf(2)),
      Leaf(5))
  )

  def correctTreeSize(s: Tree[Int] => Int) = {
    s(testTree) should be (4)
  }

  "size" should "get size right" in {
    correctTreeSize(Chapter3.size)
  }

  "sizeFold" should "get size right" in {
    correctTreeSize(Chapter3.sizeFold)
  }

  def correctMax(m: Tree[Int] => Int) = {
    m(testTree) should be (5)
  }

  "max" should "get max right" in {
    correctMax(Chapter3.max)
  }

  "maxFold" should "get max right" in {
    correctMax(Chapter3.maxFold)
  }

  def correctDepth(d: Tree[Int] => Int) = {
    d(testTree) should be (3)
  }

  "depth" should "get depth right" in {
    correctDepth(Chapter3.depth)
  }

  "depthFold" should "get depth right" in {
    correctDepth(Chapter3.depthFold)
  }

  def correctMap(m: Tree[Int] => (Int => String) => Tree[String]) = {
    def rangeToCh3List(r: Range) = r.toList.foldLeft(Nil: List[Int]){(l: List[Int], x:Int) => Cons(x, l)}
    m(testTree){x => foldRight[Int, String](rangeToCh3List(1 to x), ""){case (_, z) => z + "a"}} should be (Branch(
      Leaf("aa"),
      Branch(
        Branch(
          Leaf("a"),
          Leaf("aa")),
        Leaf("aaaaa"))
    ))
  }

  "map" should "transform a tree right" in {
    correctMap(Chapter3.map[Int, String])
  }

  "mapFold" should "transform a tree right" in {
    correctMap(Chapter3.mapFold[Int, String])
  }
}

