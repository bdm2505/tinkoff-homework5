package fintech.homework05

trait TweetStorage {

  def saveTweet(tweet: Tweet)

  def getTweet(id: String): Option[Tweet]

  def findTag(tag: String): Option[Seq[Tweet]]

  def updateTweet(id: String)(funUpdate: Tweet => Tweet): Option[Tweet] = {
    val result = getTweet(id).map(funUpdate)
    if (result.isDefined && result.get.id != id)
      return None
    result foreach saveTweet
    result
  }
}

object TweetStorage {
  def apply(): TweetStorage = new MapTweetStorage
}
