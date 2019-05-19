package homeworktwo

import org.scalatest.{FlatSpec, Matchers}
import org.scalamock.scalatest.MockFactory

import homeworktwo.Validated.RichOption

class ValidatedTest extends FlatSpec with Matchers with MockFactory {
  "isValid" should "return true for a valid instance" in {
    Valid(13).isValid shouldEqual true
  }

  it should "return false for an invalid instance" in {
    Invalid("error").isValid shouldEqual false
  }

  "getOrElse" should "return value for a valid instance" in {
    Valid(13).getOrElse(11) shouldEqual 13
  }

  it should "return default for an invalid instance" in {
    Invalid("error").getOrElse(11) shouldEqual 11
  }

  "orElse" should "return same instance for a valid instance" in {
    Valid(13).orElse(Valid(11)) shouldEqual Valid(13)
  }

  it should "return default instance for an invalid instance" in {
    Invalid("error").orElse(Valid(11)) shouldEqual Valid(11)
  }

  "zip" should "combine valid instances" in {
    Valid(13).zip(Valid(11)) shouldEqual Valid((13, 11))
  }

  it should "combine errors from invalid instances" in {
    Invalid("first error").zip(Invalid("second error")) shouldEqual Invalid(Chain("first error", "second error"))
  }

  it should "return errors from invalid instance when zipping with a valid one" in {
    Invalid("error").zip(Valid(13)) shouldEqual Invalid("error")
  }

  "map" should "apply callback to the value for a valid instance" in {
    Valid(13).map(_ * 2) shouldEqual Valid(26)
  }

  it should "return same instance for an invalid instance" in {
    val invalidInstance: Validated[String, Int] = Invalid("error")

    invalidInstance.map(_ * 2) shouldEqual Invalid("error")
  }

  "map2" should "return valid instance with correct value" in {
    Valid(13).map2(Valid(11))(_ * 2 + _ * 3) shouldEqual Valid(59)
  }

  it should "combine errors from invalid instances" in {
    val firstInvalidInstance: Validated[String, Int] = Invalid("first error")
    val secondInvalidInstance: Validated[String, Int] = Invalid("second error")

    firstInvalidInstance.map2(secondInvalidInstance)(_ * 2 + _ * 3) shouldEqual Invalid(Chain("first error", "second error"))
  }

  it should "return errors from invalid instance when mapping with a valid one" in {
    val invalidInstance: Validated[String, Int] = Invalid("error")

    invalidInstance.map2(Valid(13))(_ * 2 + _ * 3) shouldEqual Invalid("error")
  }

  "flatMap" should "transform valid instance correctly" in {
    Valid(13).flatMap(value => Valid(value * 2)) shouldEqual Valid(26)
  }

  it should "return same instance for an invalid instance" in {
    val invalidInstance: Validated[String, Int] = Invalid("error")

    invalidInstance.flatMap(value => Valid(value * 2)) shouldEqual Invalid("error")
  }

  "fold" should "execute correct callback for a valid instance" in {
    val validMockCallback = mockFunction[Int, Unit]
    val invalidMockCallback = mockFunction[Chain[String], Unit]

    validMockCallback.expects(13).once
    invalidMockCallback.expects(*).never

    Valid(13).fold(invalidMockCallback, validMockCallback)
  }

  it should "execute correct callback for an invalid instance" in {
    val validMockCallback = mockFunction[Int, Unit]
    val invalidMockCallback = mockFunction[Chain[String], Unit]

    validMockCallback.expects(*).never
    invalidMockCallback.expects(Chain("error")).once

    Invalid("error").fold(invalidMockCallback, validMockCallback)
  }

  "foreach" should "execute callback for valid instance" in {
    val mockCallback = mockFunction[Int, Unit]

    mockCallback.expects(13).once

    Valid(13).foreach(mockCallback)
  }

  it should "not execute callback for invalid instance" in {
    val mockCallback = mockFunction[Int, Unit]

    mockCallback.expects(*).never

    Invalid("error").foreach(mockCallback)
  }

  "sequence" should "return a valid instance with list of values when all values are valid" in {
    Validated.sequence(List(Valid(13), Valid(11))) shouldEqual Valid(List(13, 11))
  }

  it should "return an invalid instance with errors when some values are invalid" in {
    Validated.sequence(List(Invalid("first error"), Valid(13), Invalid("second error"))) shouldEqual Invalid(Chain("first error", "second error"))
  }

  "toValidated" should "return valid instance when option has value" in {
    val option: Option[Int] = Some(13)

    option.toValidated("error") shouldEqual Valid(13)
  }

  it should "return invalid instance when option does not have value" in {
    val option: Option[Int] = None

    option.toValidated("error") shouldEqual Invalid("error")
  }
}
