package fintech.homework05

import org.scalatest.{FlatSpec, Matchers}

class TweetStorageSpec extends FlatSpec with Matchers {

  trait Init {
    val ts = TweetStorage()
  }

  it should "save Tweets" in new Init {
    ts saveTweet Tweet("id 1", "name1", "text1")
    ts getTweet "id 1" shouldEqual Some(Tweet("id 1", "name1", "text1"))
  }

  it should "update Tweets" in new Init {
    ts saveTweet Tweet("id 1", "name1", "text1")
    ts saveTweet Tweet("id 2", "name2", "text2")

    ts.updateTweet("id 1")(_.copy(user = "name3")) shouldEqual Some(Tweet("id 1", "name3", "text1"))
    ts.updateTweet("id 2")(_.copy(likes = 14)) shouldEqual Some(Tweet("id 2", "name2", "text2", likes = 14))
    ts.updateTweet("unknown id")(_.copy(user = "name3")) shouldEqual None
    ts.updateTweet("id 1")(_.copy(id = "new id 1")) shouldEqual None
  }

  it should "find Tags" in new Init {
    val t1 = Tweet("id 1", "name1", "text1", Seq("tag1", "tag2"))
    val t2 = Tweet("id 2", "name2", "text2", Seq("tag1", "tag3"))
    ts saveTweet t1
    ts saveTweet t2

    ts findTag "tag1" shouldEqual Some(Seq(t1, t2))
    ts findTag "tag2" shouldEqual Some(Seq(t1))
    ts findTag "unknown tag" shouldEqual None
  }
}
