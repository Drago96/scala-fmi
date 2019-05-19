package homeworktwo

import scala.util.Try

import Extensions._

case class RegistrationForm(name: String,
                            email: String,
                            password: String,
                            passwordConfirmation: String,
                            birthYear: String,
                            birthMonth: String,
                            birthDay: String,
                            postalCode: String)

sealed trait RegistrationFormError

case object NameIsEmpty extends RegistrationFormError

case class InvalidEmail(email: String) extends RegistrationFormError

case object PasswordTooShort extends RegistrationFormError

case object PasswordRequiresGreaterSymbolVariety extends RegistrationFormError

case object PasswordsDoNotMatch extends RegistrationFormError

case class InvalidBirthdayDate(dateErrors: Chain[DateError]) extends RegistrationFormError

case class BirthdayDateIsInTheFuture(date: Date) extends RegistrationFormError

case class InvalidPostalCode(code: String) extends RegistrationFormError

sealed trait DateError

case class YearIsNotAnInteger(year: String) extends DateError

case class MonthIsNotAnInteger(month: String) extends DateError

case class DayIsNotAnInteger(day: String) extends DateError

case class MonthOutOfRange(month: Int) extends DateError

case class DayOutOfRange(day: Int) extends DateError

case class InvalidDate(year: Int, month: Int, day: Int) extends DateError

case class Email(user: String, domain: String)

case class User(name: String,
                email: Email,
                passwordHash: String,
                birthday: Date,
                postalCode: Option[String])

object Extensions {

  implicit class RichString(val input: String) {
    def isValidInteger: Boolean = Try(input.toInt).isSuccess
  }

  implicit class RichInt(val input: Int) {
    def isBetween(start: Int, end: Int): Boolean = input >= start && input <= end
  }

}

object UserValidator {
  def validateUsername(username: String): Validated[NameIsEmpty.type, String] =
    if (username.nonEmpty)
      Valid(username)
    else
      Invalid(NameIsEmpty)

  def validateEmail(email: String): Validated[InvalidEmail, Email] = {
    val emailPattern = "^(.+)@(.+)$".r

    email match {
      case emailPattern(user, domain) => Valid(Email(user, domain))
      case _ => Invalid(InvalidEmail(email))
    }
  }

  def validatePassword(password: String, passwordConfirmation: String): Validated[RegistrationFormError, String] =
    (
      validatePasswordLength(password),
      validatePasswordSymbolVariety(password),
      validatePasswordConfirmation(password, passwordConfirmation)
    ).zipMap((_, _, _) => PasswordUtils.hash(password))

  def validateBirthDate(year: String, month: String, day: String, today: Date): Validated[RegistrationFormError, Date] =
    (
      DateValidator.validateYearFormat(year),
      DateValidator.validateMonthFormat(month).flatMap(month => DateValidator.validateMonthRange(month)),
      DateValidator.validateDayFormat(day).flatMap(day => DateValidator.validateDayRange(day))
    ).zip
      .flatMap(date => DateValidator.validateDateFormat(date._1, date._2, date._3))
      .fold(
        errors => Invalid(InvalidBirthdayDate(errors)),
        date => validateBirthDateIsBeforeToday(date, today)
      )

  def validatePostalCode(postalCode: String, verifier: String => Boolean): Validated[InvalidPostalCode, Option[String]] =
    if (postalCode.isEmpty)
      Valid(None)
    else if (verifier(postalCode))
      Valid(Some(postalCode))
    else
      Invalid(InvalidPostalCode(postalCode))

  private def validatePasswordLength(password: String) =
    if (password.length >= 8)
      Valid(password)
    else
      Invalid(PasswordTooShort)

  private def validatePasswordSymbolVariety(password: String) =
    if (password.exists(_.isDigit) && password.exists(_.isLetter) && password.exists(!_.isLetterOrDigit))
      Valid(password)
    else
      Invalid(PasswordRequiresGreaterSymbolVariety)

  private def validatePasswordConfirmation(password: String, confirmPassword: String) =
    if (password == confirmPassword)
      Valid(password)
    else
      Invalid(PasswordsDoNotMatch)

  private def validateBirthDateIsBeforeToday(date: Date, today: Date) =
    if (date.isBefore(today))
      Valid(date)
    else
      Invalid(BirthdayDateIsInTheFuture(date))
}

object DateValidator {
  def validateYearFormat(year: String): Validated[YearIsNotAnInteger, Int] =
    if (year.isValidInteger)
      Valid(year.toInt)
    else
      Invalid(YearIsNotAnInteger(year))

  def validateMonthFormat(month: String): Validated[MonthIsNotAnInteger, Int] =
    if (month.isValidInteger)
      Valid(month.toInt)
    else
      Invalid(MonthIsNotAnInteger(month))

  def validateDayFormat(day: String): Validated[DayIsNotAnInteger, Int] =
    if (day.isValidInteger)
      Valid(day.toInt)
    else
      Invalid(DayIsNotAnInteger(day))

  def validateMonthRange(month: Int): Validated[MonthOutOfRange, Int] =
    if (month.isBetween(1, 12))
      Valid(month)
    else
      Invalid(MonthOutOfRange(month))

  def validateDayRange(day: Int): Validated[DayOutOfRange, Int] =
    if (day.isBetween(1, 31))
      Valid(day)
    else
      Invalid(DayOutOfRange(day))

  def validateDateFormat(year: Int, month: Int, day: Int): Validated[InvalidDate, Date] = Date.applyOption(year, month, day) match {
    case Some(date) => Valid(date)
    case None => Invalid(InvalidDate(year, month, day))
  }
}

object UserRegistration {
  def registerUser(userCountryPostalCodeVerifier: String => Boolean, today: Date)
                  (form: RegistrationForm): Validated[RegistrationFormError, User] =
    (
      UserValidator.validateUsername(form.name),
      UserValidator.validateEmail(form.email),
      UserValidator.validatePassword(form.password, form.passwordConfirmation),
      UserValidator.validateBirthDate(form.birthYear, form.birthMonth, form.birthDay, today),
      UserValidator.validatePostalCode(form.postalCode, userCountryPostalCodeVerifier)
    ).zipMap(User.apply)
}
