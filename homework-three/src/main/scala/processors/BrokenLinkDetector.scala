package homeworkthree.processors

import homeworkthree.Processor
import homeworkthree.http.HttpResponse
import homeworkthree.math.Monoid

import scala.concurrent.Future

object BrokenLinkDetector extends Processor[Set[String]] {
  def apply(url: String, response: HttpResponse): Future[Set[String]] = Future.successful {
    if(response.status == 404)
      Set(url)
    else
      Set.empty
  }

  implicit val brokenLinkMonoid = new Monoid[Set[String]] {
    def op(a: Set[String], b: Set[String]): Set[String] = a ++ b

    val identity: Set[String] = Set.empty
  }
}
