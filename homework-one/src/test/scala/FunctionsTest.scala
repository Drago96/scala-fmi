import scala.annotation.tailrec
import org.scalatest.{FlatSpec, Matchers}

import Functions._

class FunctionsTest extends FlatSpec with Matchers {
  "fromDigits" should "form a decimal number" in {
    fromDigits(List(1, 2, 3)) shouldBe 123
  }

  it should "form a hex number" in {
    fromDigits(List(1, 12, 4), 16) shouldBe 452
  }

  it should "work with an empty List" in {
    fromDigits(Nil) shouldBe 0
  }

  "parseInteger" should "parse a decimal number" in {
    parseInteger("123") shouldBe 123
  }

  it should "parse a hex number" in {
    parseInteger("1C4", 16) shouldBe 452
  }

  it should "parse a negative decimal number" in {
    parseInteger("-123") shouldBe -123
  }

  it should "parse a negative hex number" in {
    parseInteger("-1C4", 16) shouldBe -452
  }

  it should "parse an empty string" in {
    parseInteger("") shouldBe 0
  }

  "zipMap" should "transform two lists" in {
    zipMap[Int, Int, Int](List(1, 2, 3), List(4, 5, 6), _ * _) shouldBe List(4, 10, 18)
  }

  it should "transform lists of different length" in {
    zipMap[Int, Int, Int](List(1, 2), List(3, 4, 5), _ * _) shouldBe List(3, 8)
  }

  it should "transform empty lists" in {
    zipMap[Int, Int, Int](Nil, Nil, _ * _) shouldBe Nil
  }

  "countCoinChangeVariants" should "count the ways to give a change" in {
    countCoinChangeVariants(List(1, 2, 5), 6) shouldBe 5
  }

  it should "work when change is zero" in {
    countCoinChangeVariants(List(1, 2, 5), 0) shouldBe 1
  }

  it should "work when change is a negative number" in {
    countCoinChangeVariants(List(1, 2, 5), -13) shouldBe 0
  }

  it should "work when coin variants are an empty list" in {
    countCoinChangeVariants(Nil, 6) shouldBe 0
  }

  private def neighbours(current: Int): List[Int] = current match {
    case 1 => List(2, 5, 8)
    case 2 => List(1, 3, 6)
    case 3 => List(2, 4)
    case 4 => List(3)
    case 5 => List(6)
    case 6 => List(7)
    case 8 => List(9)
    case _ => List()
  }

  @tailrec
  private def pathToList(path: Queue[Int], accumulated: List[Int] = Nil): List[Int] =
    if (path.isEmpty) {
      accumulated.reverse
    } else {
      val current = path.peek

      pathToList(path.pop, current :: accumulated)
    }


  "bfsTraversal" should "traverse correctly when end is reachable" in {
    val path = bfsTraversal(1, 6, neighbours)

    pathToList(path) shouldBe List(1, 2, 5, 8, 3, 6)
  }

  it should "traverse correctly when end is unreachable" in {
    val path = bfsTraversal(5, 1, neighbours)

    pathToList(path) shouldBe List(5, 6, 7)
  }

  it should "traverse correctly when starting vertex has no neighbours" in {
    val path = bfsTraversal(13, 11, neighbours)

    pathToList(path) shouldBe List(13)
  }
}
