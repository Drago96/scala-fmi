import org.scalatest.{FlatSpec, Matchers}

class QueueTest extends FlatSpec with Matchers {
  "an empty queue" should "produce a queue with a single element when that element is added to it" in {
    Queue.empty[Int].push(42).toList shouldBe List(42)
  }

  it should "throw a NoSuchElementException on peek" in {
    intercept[NoSuchElementException] {
      Queue.empty[AnyVal].peek
    }
  }

  it should "throw a NoSuchElementException on pop" in {
    intercept[NoSuchElementException] {
      Queue.empty[AnyVal].pop
    }
  }

  it should "return true on isEmpty" in {
    Queue.empty[AnyVal].isEmpty shouldBe true
  }

  it should "return 0 on size" in {
    Queue.empty[AnyVal].size shouldBe 0
  }

  it should "allow consecutive push and pop operations" in {
    Queue.empty[Int].push(42).pop.toList shouldBe Nil
  }

  "a non empty queue" should "produce a new queue on pop" in {
    Queue(List(11, 13)).pop.toList shouldBe List(13)
  }

  it should "produce a new queue on push" in {
    Queue(List(11, 13)).push(42).toList shouldBe List(11, 13, 42)
  }

  it should "return the front of the queue on peek" in {
    Queue(List(11, 13)).peek shouldBe 11
  }

  it should "return false on isEmpty" in {
    Queue(List(11, 13)).isEmpty shouldBe false
  }

  it should "return correct size on size" in {
    Queue(List(11, 13)).size shouldBe 2
  }
  it should "allow push of a non empty sequence" in {
    Queue(List(11, 13)).push(List(1, 2, 3)).toList shouldBe List(11, 13, 1, 2, 3)
  }

  it should "allow push of an empty sequence" in {
    Queue(List(11, 13)).push(List.empty).toList shouldBe List(11, 13)
  }

}
