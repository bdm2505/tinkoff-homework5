package fintech.homework05

import java.time.Instant
import java.util.UUID

/**
  * Вам необходимо реализовать api для создания твиттов, получения твитта и лайка твитта
  *
  * Создание твитта:
  * На вход к вам поступает CreateTweetRequest, из которого вы должны создать объект Tweet, обязательно проверив длину
  * текста (а может быть потом появятся и другие проверки).
  * hashTags вычисляется из tweet.text, собирая все слова, начинающиеся с символа `#`.
  * tweet.id генерируется из `UUID.randomUUID.toString`.
  * При условии прохождения всех проверок твит сохраняется в базу данных (вы можете реализовать любой способ сохранения:
  * в памяти, запись в файл, или то, что вам захочется).
  * После выполнения всех операций должен вернуться созданный объект.
  *
  * Получение твитта:
  * На вход к вам поступает GetTweetRequest, вернуть вы должны объект tweet, если он найдем по id.
  *
  * Лайк твитта:
  * Должен обновлять количество лайков у твитта и возвращать новое значение.
  * Если твит не найдет, то должна возвращаться ошибка
  *
  *
  * Все функции должны возвращать значение типа Result[T]
  * в котором может лежать либо текст ошибки, либо результат выполнение
  */

case class Tweet(id: String,
                 user: String,
                 text: String,
                 hashTags: Seq[String] = Seq.empty,
                 createdAt: Option[Instant] = None,
                 likes: Int = 0)

case class CreateTweetRequest(user: String, text: String)

case class GetTweetRequest(id: String)

case class LikeRequest(id: String)

case class FindTagRequest(tag: String)


class TweetApi(storage: TweetStorage) {
  val tweetMaxLength = 100

  def createTweet(request: CreateTweetRequest): Response[Tweet] = {
    if (isCorrect(request.text)) {
      val tweet = Tweet(nextID, request.user, request.text, getTags(request.text), Some(Instant.now()))
      storage.saveTweet(tweet)
      Success(tweet)
    } else {
      Failure("text is not correct")
    }
  }

  def getTweet(request: GetTweetRequest): Response[Tweet] = {
    storage getTweet request.id map
      Success[Tweet] getOrElse
      Failure(s"no tweet with this id=${request.id}")
  }

  def likeTweet(request: LikeRequest): Response[Tweet] = {
    storage.updateTweet(request.id) { t => t.copy(likes = t.likes + 1) } map
      Success[Tweet] getOrElse
      Failure(s"no tweet with this id=${request.id}")
  }

  def findTags(request: FindTagRequest): Response[Seq[Tweet]] = {
    storage findTag request.tag map
      Success[Seq[Tweet]] getOrElse
      Failure(s"No tweets tagged '${request.tag}'")
  }

  private def getTags(text: String): Seq[String] = {
    text split " " filter { e =>
      e.startsWith("#") && e.length > 1
    } map (_.tail)
  }

  private def isCorrect(text: String): Boolean = {
    text.length <= tweetMaxLength
  }

  private def nextID: String = UUID.randomUUID.toString
}

object TweetApiExample extends App {


  val storage: TweetStorage = TweetStorage()
  val app = new TweetApi(storage)

  val request = CreateTweetRequest(user = "me", text = "Hello, world!")

  val response = app.createTweet(request)
  response match {
    case Success(value) => println(s"Created tweet with id: ${value.id}")
    case Failure(message) => println(s"Failed to create tweet: $message")
  }

}
