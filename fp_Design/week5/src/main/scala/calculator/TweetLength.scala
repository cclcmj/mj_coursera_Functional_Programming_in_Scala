package calculator

object TweetLength extends TweetLengthInterface {
  final val MaxTweetLength = 140

  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] = {
    Signal {
      val str = tweetText()
      MaxTweetLength - tweetLength(str)
    }
  }

  def colorForRemainingCharsCount(
      remainingCharsCount: Signal[Int]
  ): Signal[String] = {
    Signal {
      val remaining = remainingCharsCount()
      if (remaining >= 15) "green"
      else if (remaining >= 0 && remaining <= 14) "orange"
      else "red"
    }
  }

  /** Computes the length of a tweet, given its text string.
    *  This is not equivalent to text.length, as tweet lengths count the number
    *  of Unicode *code points* in the string.
    *  Note that this is still a simplified view of the reality. Full details
    *  can be found at
    *  https://dev.twitter.com/overview/api/counting-characters
    */
  private def tweetLength(text: String): Int = {
    /* This should be simply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if (text.isEmpty) 0
    else {
      text.length - text.init
        .zip(text.tail)
        .count((Character.isSurrogatePair _).tupled)
    }
  }
}