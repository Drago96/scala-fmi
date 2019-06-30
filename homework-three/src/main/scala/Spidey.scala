package homeworkthree

import homeworkthree.html.HtmlUtils
import homeworkthree.http._
import homeworkthree.math.Monoid

import scala.concurrent.{ExecutionContext, Future}

case class SpideyNode[O: Monoid](processedResponse: Future[O], visitedUrls: Set[String], urlsToVisit: List[String])

case class SpideyConfig(maxDepth: Int,
                        sameDomainOnly: Boolean = true,
                        tolerateErrors: Boolean = true,
                        retriesOnError: Int = 0)

class Spidey(httpClient: HttpClient)(implicit ex: ExecutionContext) {
  def crawl[O: Monoid](url: String, config: SpideyConfig)
                      (processor: Processor[O]): Future[O] = {
    crawl(initialNode(url), config)(processor)
  }

  private def crawl[O: Monoid](accumulatedNode: SpideyNode[O], config: SpideyConfig, currentDepth: Int = 0)
                              (processor: Processor[O]): Future[O] =
    if (currentDepth > config.maxDepth || accumulatedNode.urlsToVisit.isEmpty) {
      accumulatedNode.processedResponse
    } else {
      generateNodesFromUrls(accumulatedNode.urlsToVisit, config)(processor)
        .map(combineChildNodes(_))
        .map(mergeNodeWithAccumulated(_, accumulatedNode))
        .flatMap(crawl(_, config, currentDepth + 1)(processor))
    }

  private def initialNode[O: Monoid](url: String) = SpideyNode(Future.successful(Monoid[O].identity), Set.empty, List(url))

  private def emptyNode[O: Monoid](url: String) = SpideyNode(Future.successful(Monoid[O].identity), Set(url), List.empty)

  private def generateNodesFromUrls[O: Monoid](urls: List[String], config: SpideyConfig)
                                              (processor: Processor[O]): Future[List[SpideyNode[O]]] = {
    Future.sequence(
      urls.filter(HttpUtils.isValidHttp).map(url => {
        generateNodeFromUrl(url, config)(processor)
      })
    )
  }

  private def generateNodeFromUrl[O: Monoid](url: String, config: SpideyConfig, retryAttempt: Int = 0)
                                            (processor: Processor[O]): Future[SpideyNode[O]] = {
    httpClient
      .get(url)
      .flatMap(response => {
        if (response.isServerError && config.retriesOnError > retryAttempt)
          generateNodeFromUrl(url, config, retryAttempt + 1)(processor)
        else
          Future.successful(generateNodeFromSuccessfulResponse(url, response, config)(processor))
      })
      .recoverWith {
        case _ if config.retriesOnError > retryAttempt => generateNodeFromUrl(url, config, retryAttempt + 1)(processor)
        case _ if config.tolerateErrors => Future.successful(emptyNode(url))
      }
  }

  private def generateNodeFromSuccessfulResponse[O: Monoid](url: String, response: HttpResponse, config: SpideyConfig)
                                                           (processor: Processor[O]): SpideyNode[O] = {
    SpideyNode(
      processor(url, response).recover {
        case _ if config.tolerateErrors => Monoid[O].identity
      },
      Set(url),
      HtmlUtils.linksOf(response.body, url).filter(!config.sameDomainOnly || HttpUtils.sameDomain(url, _))
    )
  }

  private def combineChildNodes[O: Monoid](nodes: List[SpideyNode[O]]): SpideyNode[O] = {
    SpideyNode(
      combineProcessedResponses(nodes.map(_.processedResponse)),
      nodes.map(_.visitedUrls).foldLeft(Set[String]())(_.union(_)),
      nodes.map(_.urlsToVisit).foldLeft(List[String]())(_ ++ _)
    )
  }

  private def mergeNodeWithAccumulated[O: Monoid](node: SpideyNode[O], accumulated: SpideyNode[O]): SpideyNode[O] = {
    val visitedUrls = accumulated.visitedUrls ++ node.visitedUrls

    SpideyNode(
      combineProcessedResponses(List(accumulated.processedResponse, node.processedResponse)),
      visitedUrls,
      node.urlsToVisit.filter(!visitedUrls(_)).distinct
    )
  }

  private def combineProcessedResponses[O: Monoid](processedResponses: List[Future[O]]) = {
    Future
      .sequence(processedResponses)
      .map(responses =>
        responses.fold(Monoid[O].identity)(Monoid[O].op(_, _))
      )
  }
}
