package homeworkthree

import java.util.concurrent.ForkJoinPool

import homeworkthree.http.AsyncHttpClient
import homeworkthree.processors.{BrokenLinkDetector, FileOutput, WordCounter}
import homeworkthree.processors.WordCounter.wordCountMonoid
import homeworkthree.processors.BrokenLinkDetector.brokenLinkMonoid
import homeworkthree.processors.FileOutput.savedFilesMonoid

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object SpideyApp {
  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(new ForkJoinPool)
  val blockingExecutionContext: ExecutionContext = ExecutionContext.fromExecutor(new ForkJoinPool(4))

  val httpClient = new AsyncHttpClient
  val spidey = new Spidey(httpClient)

  def printUsage: Unit = {
    println(
      """
        |Usage:
        |
        |SpideyApp <url> <max-depth> <processor> [processor-config]
        |
        |Possible processors and their config are:
        |
        |file-output <target-dir>
        |word-counter
        |broken-link-detector
      """.stripMargin)
  }

  def main(args: Array[String]): Unit = {
    val config = SpideyConfig(3, true, true, 3)

    val crawls = Future.sequence(
      List(
        spidey.crawl("https://abv.bg/", config)(WordCounter),
        spidey.crawl("https://abv.bg/", config)(FileOutput("D:\\test")(blockingExecutionContext)),
        spidey.crawl("https://abv.bg/", config)(BrokenLinkDetector)
      )
    )

    Await.result(crawls, Duration.Inf)

    crawls.foreach(println(_))

    httpClient.shutdown()
  }
}
