package homeworkthree.http

import scala.concurrent.Future

trait HttpClient {
  def get(url: String): Future[HttpResponse]
}
