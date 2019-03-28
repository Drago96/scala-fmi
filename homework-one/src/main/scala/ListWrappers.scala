object ListWrappers {

  implicit class TakeUntil[T](list: List[T]) {
    def takeUntil(predicate: T => Boolean): List[T] = {
      list.span(predicate) match {
        case (head, tail) => head ::: tail.take(1)
      }
    }
  }

}
