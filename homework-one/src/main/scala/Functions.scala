import scala.annotation.tailrec

import ListWrappers.TakeUntil

object Functions {
  def fromDigits(digits: List[Int], radix: Int = 10): Int = digits.reverse.zipWithIndex.foldLeft(0) {
    (sum, digit) => {
      val (value, index) = digit

      sum + value * math.pow(radix, index).toInt
    }
  }

  def parseInteger(integer: String, radix: Int = 10): Int = integer.toList match {
    case '-' :: _ => -parseInteger(integer.tail, radix)
    case positiveInteger => fromDigits(positiveInteger.map({
      case symbol if symbol.isDigit => symbol.asDigit
      case letter => letter.toInt - 55
    }), radix)
  }

  def zipMap[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] = {
    @tailrec
    def zip(a: List[A], b: List[B], accumulator: List[(A, B)] = Nil): List[(A, B)] =
      if (a.isEmpty || b.isEmpty) {
        accumulator.reverse
      } else {
        zip(a.tail, b.tail, (a.head, b.head) :: accumulator)
      }

    @tailrec
    def map(a: List[(A, B)], accumulator: List[C] = Nil)(f: (A, B) => C): List[C] =
      if (a.isEmpty) {
        accumulator.reverse
      } else {
        map(a.tail, f(a.head._1, a.head._2) :: accumulator)(f)
      }

    map(zip(a, b))(f)
  }

  def countCoinChangeVariants(denominations: List[Int], change: Int): Int = {
    def countCoinChangeVariants(denominations: List[Int], change: Int, coinOptions: Int): Int =
      if (change == 0) {
        1
      } else if (change < 0 || coinOptions == 0) {
        0
      } else {
        countCoinChangeVariants(denominations, change, coinOptions - 1) +
          countCoinChangeVariants(denominations, change - denominations(coinOptions - 1), coinOptions)
      }

    countCoinChangeVariants(denominations, change, denominations.length)
  }

  def bfsTraversal(start: Int, end: Int, neighbours: Int => List[Int]): Queue[Int] = {
    val visited = Set[Int](start)

    val verticesToVisit = Queue[Int](List[Int](start))
    val path = verticesToVisit

    @tailrec
    def bfsTraversal(verticesToVisit: Queue[Int], path: Queue[Int], visited: Set[Int]): Queue[Int] = {
      if (verticesToVisit.isEmpty) {
        return path
      }

      val current = verticesToVisit.peek

      if (current == end) {
        return path
      }

      val verticesToExplore = neighbours(current).filter(!visited(_)).takeUntil(_ != end)

      if (verticesToExplore.isEmpty) {
        return path
      }

      val newVerticesToVisit = verticesToExplore.foldLeft(verticesToVisit.pop)(_.push(_))
      val newPath = verticesToExplore.foldLeft(path)(_.push(_))

      bfsTraversal(newVerticesToVisit, newPath, visited ++ verticesToExplore)
    }

    bfsTraversal(verticesToVisit, path, visited)
  }
}
