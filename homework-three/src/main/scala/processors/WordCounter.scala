package homeworkthree.processors

import homeworkthree.Processor
import homeworkthree.html.HtmlUtils
import homeworkthree.http.HttpResponse
import homeworkthree.math.Monoid

import scala.concurrent.Future

case class WordCount(wordToCount: Map[String, Int])

object WordCount {
  def wordsOf(text: String): List[String] = text.split("\\W+").toList.filter(_.nonEmpty)
}

object WordCounter extends Processor[WordCount] {
  def apply(url: String, response: HttpResponse): Future[WordCount] = Future.successful {
    val responseText = HtmlUtils.toText(response.body)

    WordCount.wordsOf(responseText).foldLeft(WordCount(Map.empty))((accumulatedWordCount, currentWord) => {
      WordCount(accumulatedWordCount.wordToCount + (currentWord -> countOccurrences(currentWord, responseText)))
    })
  }

  implicit val wordCountMonoid = new Monoid[WordCount] {
    def op(a: WordCount, b: WordCount): WordCount = {
      WordCount(
        (a.wordToCount.keys ++ b.wordToCount.keys)
          .map(key => key -> (a.wordToCount.getOrElse(key, 0) + b.wordToCount.getOrElse(key, 0)))
          .toMap
      )
    }

    val identity: WordCount = WordCount(Map.empty)
  }

  private def countOccurrences(source: String, target: String): Int =
    source.sliding(target.length).count(_ == source)
}
