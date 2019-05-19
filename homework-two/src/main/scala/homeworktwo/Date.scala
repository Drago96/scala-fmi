package homeworktwo

import java.time.LocalDate

import scala.util.Try

case class Date(year: Int, month: Int, day: Int) {
  def isBefore(date: Date): Boolean =
    if (year != date.year)
      year < date.year
    else if (month != date.month)
      month < date.month
    else
      day <= date.day
}

object Date {
  def applyOption(year: Int, month: Int, day: Int): Option[Date] = Try {
    LocalDate.of(year, month, day)

    Date(year, month, day)
  }.toOption
}
