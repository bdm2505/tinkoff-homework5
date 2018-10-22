package fintech.homework05

import scala.collection.mutable

class MapTweetStorage extends TweetStorage {
  val mapID: mutable.Map[String, Tweet] = mutable.Map.empty
  val mapTags: mutable.Map[String, Seq[Tweet]] = mutable.Map.empty

  def saveTweet(tweet: Tweet): Unit = {
    mapID += (tweet.id -> tweet)
    tweet.hashTags foreach { tag =>
      if (mapTags contains tag)
        mapTags update(tag, mapTags(tag) :+ tweet)
      else
        mapTags += (tag -> Seq(tweet))
    }
  }

  def getTweet(id: String): Option[Tweet] = {
    mapID.get(id)
  }

  def findTag(tag: String): Option[Seq[Tweet]] = {
    if (mapTags contains tag)
      Some(mapTags(tag))
    else
      None
  }
}
