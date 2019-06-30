package homeworkthree

import homeworkthree.http.HttpResponse

import scala.concurrent.Future

trait Processor[O] {
  def apply(url: String, response: HttpResponse): Future[O]
}
