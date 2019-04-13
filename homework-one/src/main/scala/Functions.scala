import scala.annotation.tailrec

object Functions {
  def fromDigits(digits: List[Int], radix: Int = 10): Int = digits.foldLeft(0)(_ * radix + _)

  def parseInteger(integer: String, radix: Int = 10): Int = integer.toList match {
    case '-' :: _ => -parseInteger(integer.tail, radix)
    case positiveInteger => fromDigits(positiveInteger.map(_.asDigit), radix)
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
    if (change == 0) 1
    else if (change < 0 || denominations.isEmpty) 0
    else countCoinChangeVariants(denominations, change - denominations.head) +
      countCoinChangeVariants(denominations.tail, change)
  }

  def bfsTraversal(start: Int, end: Int, neighbours: Int => List[Int]): Queue[Int] = {
    @tailrec
    def bfsTraversal(verticesToVisit: Queue[Int], visited: Set[Int], path: Queue[Int]): Queue[Int] = {
      if (verticesToVisit.isEmpty) {
        return path
      }

      val current = verticesToVisit.peek

      if (current == end) {
        return path.push(current)
      }

      val verticesToExplore = neighbours(current).filter(!visited(_))

      bfsTraversal(
        verticesToVisit.pop.push(verticesToExplore),
        visited ++ verticesToExplore,
        path.push(current)
      )
    }

    bfsTraversal(Queue(List(start)), Set(start), Queue.empty)
  }
}
