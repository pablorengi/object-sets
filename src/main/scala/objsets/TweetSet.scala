package objsets

import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 */


abstract class TweetSet {
  
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet
  def union(that: TweetSet): TweetSet
  def mostRetweeted: Tweet
  def mostRetweetedAcc(tweet: Tweet): Tweet
  def descendingByRetweet: TweetList
  def incl(x: Tweet): TweetSet
  def contains(x: Tweet): Boolean
  def isEmpty: Boolean
  def remove(tweet: Tweet): TweetSet
  def foreach(f: Tweet => Unit): Unit

}

class Empty extends TweetSet {
  
  def isEmpty = true
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc
  def contains(tweet: Tweet): Boolean = false
  def mostRetweeted: Tweet = throw new java.util.NoSuchElementException()
  def mostRetweetedAcc(tweet: Tweet): Tweet = tweet
  def descendingByRetweet: TweetList = Nil
  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty(), new Empty())
  def remove(tweet: Tweet): TweetSet = this
  def foreach(f: Tweet => Unit): Unit = ()
  def union(that: TweetSet): TweetSet = that
  
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val set = left.filterAcc(p, right.filterAcc(p, acc))
    if(p(elem)) set.incl(elem) else set
  }

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  def union(that: TweetSet): TweetSet = right.union(left.union(that.incl(elem)))

  def mostRetweeted: Tweet = mostRetweetedAcc(elem)

  def mostRetweetedAcc(acc: Tweet): Tweet = left.mostRetweetedAcc(right.mostRetweetedAcc(if(elem.retweets > acc.retweets) elem else acc))

  def descendingByRetweet: TweetList = {
    def descendingByRetweet(tweets: TweetSet): TweetList =
      if(tweets.isEmpty) Nil
      else new Cons(tweets.mostRetweeted, descendingByRetweet(tweets.remove(tweets.mostRetweeted)))


    descendingByRetweet(this)
  }

  def isEmpty: Boolean = false
}

trait TweetList{

	def head: Tweet

	def tail: TweetList

	def isEmpty: Boolean

	def foreach(f: Tweet => Unit): Unit = {
			if (!isEmpty) {
				f(head)
				tail.foreach(f)
			}
	}

}

object Nil extends TweetList {
  def isEmpty = true
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google: List[String] = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple: List[String]  = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader.allTweets filter {
    (t: Tweet) => google.exists((keyword: String) => t.text.contains(keyword))
  }
  
  lazy val appleTweets: TweetSet = TweetReader.allTweets filter {
    (t: Tweet) => apple.exists((keyword: String) => t.text.contains(keyword))
  }
  
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
   lazy val trending: TweetList = (googleTweets union appleTweets).descendingByRetweet
   
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}