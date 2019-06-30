package homeworkthree.processors

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import java.util.UUID

import homeworkthree.Processor
import homeworkthree.http.HttpResponse
import homeworkthree.math.Monoid

import scala.concurrent.{ExecutionContext, Future}

case class SavedFiles(urlToPath: Map[String, Path])

class FileOutput(targetDir: String)
                (ex: ExecutionContext) extends Processor[SavedFiles] {
  private implicit val blockingExc: ExecutionContext = ex

  private val targetPath = Paths.get(targetDir)

  private def generatePathFor(url: String): Path = {
    val urlFileName = Option(Paths.get(new URI(url).getPath).getFileName).map(_.toString).getOrElse("")
    val fileName = s"${UUID.randomUUID().toString}-$urlFileName"

    targetPath.resolve(fileName)
  }

  def apply(url: String, response: HttpResponse): Future[SavedFiles] = Future {
    SavedFiles(Map(url -> Files.write(generatePathFor(url), response.bodyAsBytes)))
  }
}

object FileOutput {
  def apply(targetDir: String)(ex: ExecutionContext) = new FileOutput(targetDir)(ex)

  implicit val savedFilesMonoid = new Monoid[SavedFiles] {
    def op(a: SavedFiles, b: SavedFiles): SavedFiles = SavedFiles(a.urlToPath ++ b.urlToPath)

    val identity: SavedFiles = SavedFiles(Map.empty)
  }
}
