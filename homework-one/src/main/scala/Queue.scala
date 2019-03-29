class Queue[A] private(val in: List[A], val out: List[A]) {
  def peek: A =
    if (out.nonEmpty) out.head
    else if (in.nonEmpty) in.last
    else throw new NoSuchElementException

  def push(n: A): Queue[A] = new Queue(n :: in, out)

  def pop: Queue[A] = out match {
    case Nil if in.nonEmpty => new Queue(Nil, in.reverse.tail)
    case _ :: xs => new Queue(in, xs)
    case _ => throw new NoSuchElementException
  }

  def isEmpty: Boolean = in.isEmpty && out.isEmpty

  def size: Int = in.size + out.size
}

object Queue {
  def empty[A]: Queue[A] = new Queue(Nil, Nil)

  def apply[A](xs: Seq[A]): Queue[A] = new Queue(Nil, xs.toList)
}
