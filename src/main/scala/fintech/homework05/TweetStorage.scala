package fintech.homework05

trait TweetStorage {

  def saveTweet(tweet: Tweet)

  def getTweet(id: String): Option[Tweet]

  def findTag(tag: String): Option[Seq[Tweet]]

  def updateTweet(id: String)(funUpdate: Tweet => Tweet): Option[Tweet] = {
    getTweet(id) map funUpdate flatMap {
      case tweet: Tweet if tweet.id == id =>
        saveTweet(tweet)
        Some(tweet)
      case _ =>
        None
    }
  }
}

object TweetStorage {
  def apply(): TweetStorage = new MapTweetStorage
}
