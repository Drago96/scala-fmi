package homeworktwo

sealed trait Validated[+E, +A] {
  def isValid: Boolean = this match {
    case Valid(_) => true
    case Invalid(_) => false
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Valid(value) => value
    case Invalid(_) => default
  }

  def orElse[F >: E, B >: A](default: => Validated[F, B]): Validated[F, B] = this match {
    case Valid(_) => this
    case Invalid(_) => default
  }

  def zip[EE >: E, B](vb: Validated[EE, B]): Validated[EE, (A, B)] = (this, vb) match {
    case (Valid(a), Valid(b)) => Valid((a, b))
    case (Valid(_), Invalid(ee)) => Invalid(ee)
    case (Invalid(e), Valid(_)) => Invalid(e)
    case (Invalid(e), Invalid(ee)) => Invalid(e ++ ee)
  }

  def map[B](f: A => B): Validated[E, B] = this match {
    case Valid(value) => Valid(f(value))
    case Invalid(errors) => Invalid(errors)
  }

  def map2[EE >: E, B, R](vb: Validated[EE, B])(f: (A, B) => R): Validated[EE, R] = zip(vb).map(f.tupled(_))

  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] = this match {
    case Valid(value) => f(value)
    case Invalid(errors) => Invalid(errors)
  }

  def fold[B](invalid: Chain[E] => B, valid: A => B): B = this match {
    case Valid(value) => valid(value)
    case Invalid(errors) => invalid(errors)
  }

  def foreach(f: A => Unit): Unit = fold(_ => (), f)
}

case class Valid[+A](value: A) extends Validated[Nothing, A]

case class Invalid[+E](errors: Chain[E]) extends Validated[E, Nothing]

object Invalid {
  def apply[E](error: E): Invalid[E] = Invalid(Chain(error))
}

object Validated {
  def sequence[E, A](xs: List[Validated[E, A]]): Validated[E, List[A]] = {
    val failedValidations = xs.filter {
      case Valid(_) => false
      case Invalid(_) => true
    }

    if (failedValidations.isEmpty)
      Valid(xs.map {
        case Valid(value) => value
      })
    else
      Invalid(failedValidations.map {
        case Invalid(errors) => errors
      }.reduceLeft(_ ++ _))
  }

  implicit class ValidatedTuple2[EE, A, B](val tuple: (Validated[EE, A], Validated[EE, B])) extends AnyVal {
    def zip: Validated[EE, (A, B)] = tuple._1.zip(tuple._2)

    def zipMap[R](f: (A, B) => R): Validated[EE, R] = zip.map(f.tupled(_))
  }

  implicit class ValidatedTuple3[EE, A, B, C](val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C])) extends AnyVal {
    def zip: Validated[EE, (A, B, C)] = tuple._1.zip(tuple._2).zip(tuple._3).flatMap(
      tupledValues => Valid(tupledValues._1._1, tupledValues._1._2, tupledValues._2)
    )

    def zipMap[R](f: (A, B, C) => R): Validated[EE, R] = zip.map(f.tupled(_))
  }

  implicit class ValidatedTuple4[EE, A, B, C, D]
  (val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C], Validated[EE, D])) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D)] = tuple._1.zip(tuple._2).zip(tuple._3).zip(tuple._4).flatMap(
      tupledValues => Valid(
        tupledValues._1._1._1,
        tupledValues._1._1._2,
        tupledValues._1._2,
        tupledValues._2
      )
    )

    def zipMap[R](f: (A, B, C, D) => R): Validated[EE, R] = zip.map(f.tupled(_))
  }

  implicit class ValidatedTuple5[EE, A, B, C, D, E]
  (val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C], Validated[EE, D], Validated[EE, E])) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D, E)] = tuple._1.zip(tuple._2).zip(tuple._3).zip(tuple._4).zip(tuple._5).flatMap(
      tupledValues => Valid(
        tupledValues._1._1._1._1,
        tupledValues._1._1._1._2,
        tupledValues._1._1._2,
        tupledValues._1._2,
        tupledValues._2
      )
    )

    def zipMap[R](f: (A, B, C, D, E) => R): Validated[EE, R] = zip.map(f.tupled(_))
  }

  implicit class RichOption[A](val option: Option[A]) extends AnyVal {
    def toValidated[E](onEmpty: => E): Validated[E, A] = option match {
      case Some(a) => Valid(a)
      case None => Invalid(onEmpty)
    }
  }

}
