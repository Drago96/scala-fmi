package homeworktwo

sealed trait Chain[+A] {
  def head: A

  def tail: Option[Chain[A]]

  def isEmpty: Boolean = false

  def +:[B >: A](front: B): Chain[B] = Append(Singleton(front), this)

  def :+[B >: A](back: B): Chain[B] = Append(this, Singleton(back))

  def ++[B >: A](right: Chain[B]): Chain[B] = Append(this, right)

  def foldLeft[B](initial: B)(f: (B, A) => B): B = this match {
    case Singleton(first) => f(initial, first)
    case Append(left, right) => right.foldLeft(left.foldLeft(initial)(f))(f)
  }

  def reduceLeft[B >: A](f: (B, A) => B): B = this match {
    case Singleton(first) => first
    case Append(left, right) => right.foldLeft(left.foldLeft(left.head: B)(f))(f)
  }

  def map[B](f: A => B): Chain[B] = this match {
    case Singleton(first) => Singleton(f(first))
    case Append(left, right) => left.map(f) ++ right.map(f)
  }

  def flatMap[B](f: A => Chain[B]): Chain[B] = this match {
    case Singleton(first) => f(first)
    case Append(left, right) => left.flatMap(f) ++ right.flatMap(f)
  }

  def foreach(f: A => Unit): Unit = foldLeft(())((_, next) => f(next))

  override def equals(that: Any): Boolean = that match {
    case chain: Chain[_] => hashCode == chain.hashCode
    case _ => false
  }

  override def hashCode: Int = foldLeft(0) {
    _ * 31 + _.hashCode
  }

  override def toString: String = toList.mkString("Chain(", ",", ")")

  def toList: List[A] = foldLeft(List.empty[A])((acc, next) => next :: acc).reverse

  def toSet[B >: A]: Set[B] = foldLeft(Set.empty[B])((acc, next) => acc + next)

  def min[B >: A](implicit order: Ordering[B]): B = reduceLeft(
    (minElement, currentElement) =>
      if (order.lteq(minElement, currentElement))
        minElement
      else
        currentElement
  )

  def max[B >: A](implicit order: Ordering[B]): B = reduceLeft(
    (maxElement, currentElement) =>
      if (order.gteq(maxElement, currentElement))
        maxElement
      else
        currentElement
  )

  def listify: Chain[A] = Chain(toList.head, toList.tail: _*)
}

case class Singleton[+A](head: A) extends Chain[A] {
  def tail: Option[Chain[A]] = None
}

case class Append[+A](left: Chain[A], right: Chain[A]) extends Chain[A] {
  def head: A = left.head

  def tail: Option[Chain[A]] = left match {
    case Singleton(_) => Some(right)
    case _ => listify.tail
  }
}

object Chain {
  def apply[A](head: A, rest: A*): Chain[A] = {
    val elements = head +: rest

    elements.init.foldRight(Singleton(elements.last): Chain[A])(_ +: _)
  }

  def unapplySeq[A](chain: Chain[A]): Option[Seq[A]] = Some(chain.toList)
}
