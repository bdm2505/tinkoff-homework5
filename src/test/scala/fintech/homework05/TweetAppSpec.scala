package fintech.homework05

import org.scalatest.{FlatSpec, Matchers}

class TweetAppSpec extends FlatSpec with Matchers {

  trait Init {
    val storage = TweetStorage()
    storage saveTweet Tweet("id 10", "user10", "text 10")
    storage saveTweet Tweet("id 11", "user11", "text 11", Seq("tag1"))
    val app = new TweetApi(storage)
  }


  it should "create tweets" in new Init {
    app createTweet CreateTweetRequest("user", "text") match {
      case Success(Tweet(_, "user", "text", seq, _, _)) =>
        seq shouldEqual Seq.empty
        "ok"
    }
    app createTweet CreateTweetRequest("user", "text #tag1 #tag2 and ...") match {
      case Success(Tweet(_, _, _, Seq("tag1", "tag2"), _, _)) => "ok"
    }
    app createTweet CreateTweetRequest("user", "text" * app.tweetMaxLength) shouldBe a [Failure[_]]
  }

  it should "get tweets" in new Init {
    app getTweet GetTweetRequest("id 10") shouldEqual Success(Tweet("id 10", "user10", "text 10"))
    app getTweet GetTweetRequest("unknown id") shouldBe a [Failure[_]]
  }

  it should "like tweets" in new Init {
    app likeTweet LikeRequest("id 10") match {
      case Success(Tweet(_, "user10", "text 10", _, _, 1)) => "ok"
    }
    app likeTweet LikeRequest("unknown id") shouldBe a [Failure[_]]
  }

  it should "find tags" in new Init {
    app findTags FindTagRequest("tag1") shouldEqual Success(Seq(Tweet("id 11", "user11", "text 11", Seq("tag1"))))
    app findTags FindTagRequest("unknown tag") shouldBe a [Failure[_]]
  }
}
