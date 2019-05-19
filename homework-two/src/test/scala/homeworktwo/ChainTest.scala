package homeworktwo

import org.scalatest.{FlatSpec, Matchers}
import org.scalamock.scalatest.MockFactory

class ChainTest extends FlatSpec with Matchers with MockFactory {
  "head" should "return the element in a chain with a single element" in {
    Chain(1).head shouldEqual 1
  }

  it should "return the first element when chain has multiple elements" in {
    Chain(3, 2, 1).head shouldEqual 3
  }

  "tail" should "return None when chain has a single element" in {
    Chain(1).tail shouldEqual None
  }

  it should "return Some(Chain) with all elements except the first one when chain has multiple elements" in {
    Chain(3, 2, 1).tail shouldEqual Some(Chain(2, 1))
  }

  "isEmpty" should "return false" in {
    Chain(3, 2, 1).isEmpty shouldEqual false
  }

  "+:" should "append an element to the front of the chain" in {
    (3 +: Chain(2, 1)) shouldEqual Chain(3, 2, 1)
  }

  ":+" should "append an element to the back of the chain" in {
    (Chain(3, 2) :+ 1) shouldEqual Chain(3, 2, 1)
  }

  "++" should "append two chains" in {
    (Chain(1, 2) ++ Chain(3, 4)) shouldEqual Chain(1, 2, 3, 4)
  }

  "foldLeft" should "produce correct result for a chain with a single element" in {
    Chain(13).foldLeft(11)(_ + _) shouldEqual 24
  }

  it should "produce correct result for a chain with multiple elements" in {
    Chain(1, 2, 3).foldLeft(4)(_ * _) shouldEqual 24
  }

  "reduceLeft" should "produce correct result for a chain with a single element" in {
    Chain(13).reduceLeft(_ + _) shouldEqual 13
  }

  it should "produce correct result for a chain with multiple elements" in {
    Chain(1, 2, 3).reduceLeft(_ * _) shouldEqual 6
  }

  "map" should "produce correct result for a chain with a single element" in {
    Chain(13).map(_ * 3) shouldEqual Chain(39)
  }

  it should "work correctly for a chain with multiple elements" in {
    Chain(1, 2, 3).map(_ * 3) shouldEqual Chain(3, 6, 9)
  }

  "flatMap" should "produce correct result for a chain with a single element" in {
    Chain(13).flatMap(number => Chain(1, 2, number * 3)) shouldEqual Chain(1, 2, 39)
  }

  it should "produce correct result for a chain with multiple elements" in {
    Chain(1, 2, 3).flatMap(number => Chain(1, 2, number * 3)) shouldEqual Chain(1, 2, 3, 1, 2, 6, 1, 2, 9)
  }

  "foreach" should "run callback for each element in the chain" in {
    val mockCallback = mockFunction[Int, Unit]

    inSequence {
      mockCallback expects 1
      mockCallback expects 2
      mockCallback expects 3
    }

    Chain(1, 2, 3).foreach(mockCallback)
  }

  "toString" should "stringify a chain correctly" in {
    Chain(1, 2, 3).toString shouldEqual "Chain(1,2,3)"
  }

  "toList" should "convert a chain to list correctly" in {
    Chain(1, 2, 3).toList shouldEqual List(1, 2, 3)
  }

  "toSet" should "convert a chain to set correctly" in {
    Chain(1, 2, 3).toList shouldEqual List(1, 2, 3)
  }

  "min" should "return min element in the chain" in {
    Chain(13, 11, 1996).min shouldEqual 11
  }

  "max" should "return max element in the chain" in {
    Chain(13, 11, 1996).max shouldEqual 1996
  }

  "listify" should "not modify the chain" in {
    Chain(13, 11, 1996).listify shouldEqual Chain(13, 11, 1996)
  }
}
