package objsets

import TweetReader.*

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int):
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 */
abstract class TweetSet extends TweetSetInterface:

  // I added this
  def empty: Boolean
  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   * Question: Can we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   * This can be implemented at the higher level because it will apply the logic
   * from the helper function in the same way
   */
  def filter(p: Tweet => Boolean): TweetSet =
    filterAcc(p, new Empty)

  // This is a helper method for `filter` that propagetes the accumulated tweets
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   * I will move union into the subclasses as the implementation will be much
   * different for an empty set
   */
  def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   * Implement it here to avoid improper error handling; add helper method in
   * nonempty case only
   */
  def mostRetweeted: Tweet =
    if (empty) { throw new `IllegalArgumentException` }
    else { mostRetweets(new Tweet("initialUser", "initialText", -1)) }

  def mostRetweets(tweet: Tweet): Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

class Empty extends TweetSet:
  // I added this
  def empty: Boolean = true
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc
  // The following methods are already implemented
  def contains(tweet: Tweet): Boolean = false
  def incl(tweet: Tweet): TweetSet = NonEmpty(tweet, Empty(), Empty())
  def remove(tweet: Tweet): TweetSet = this
  def foreach(f: Tweet => Unit): Unit = ()
  // chose to add to individual classes
  def union(that: TweetSet): TweetSet = that
  def mostRetweets(max: Tweet) = max
  def descendingByRetweet: TweetList = Nil

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet:
  // I added this
  def empty: Boolean = false

  // added filter helper
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    if (p(elem)) right.filterAcc(p, left.filterAcc(p, acc.incl(elem)))
    else right.filterAcc(p, left.filterAcc(p, acc))
    // these eventually lead to calls on empty sets, which return acc
  }

  // added union method here, following logic from class
  // destructuring our "this" tree and adding the items to "that" tree
  // and then returning that set with the items from "this" added to it
  def union(that: TweetSet): TweetSet =
    left union (right union that) incl elem

  // returns the tweet with the highest number of retweets
  def mostRetweets(max: Tweet): Tweet = {
    // exit condition is in the empty set method
    if (elem.retweets > max.retweets) {
      right.mostRetweets(left.mostRetweets(elem))
    }
    else {
      right.mostRetweets(left.mostRetweets(max))
    }
  }

  // returns a TweetList sorted in descending order
  def descendingByRetweet: TweetList = {
    // Cons takes 2 params: head is a Tweet, tail is a list of Tweets
    // when we call this on an empty tree it returns Nil
    def mostPopular = this.mostRetweeted
    new Cons(mostPopular, this.remove(mostPopular).descendingByRetweet)
  }

  /** The following methods are already implemented */

  def contains(x: Tweet): Boolean =
    if x.text < elem.text then
      left.contains(x)
    else if elem.text < x.text then
      right.contains(x)
    else true

  def incl(x: Tweet): TweetSet =
    if x.text < elem.text then
      NonEmpty(elem, left.incl(x), right)
    else if elem.text < x.text then
      NonEmpty(elem, left, right.incl(x))
    else
      this

  def remove(tw: Tweet): TweetSet =
    if tw.text < elem.text then
      NonEmpty(elem, left.remove(tw), right)
    else if elem.text < tw.text then
      NonEmpty(elem, left, right.remove(tw))
    else
      left.union(right)

  def foreach(f: Tweet => Unit): Unit =
    f(elem)
    left.foreach(f)
    right.foreach(f)

trait TweetList:
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if !isEmpty then
      f(head)
      tail.foreach(f)

object Nil extends TweetList:
  def head = throw java.util.NoSuchElementException("head of EmptyList")
  def tail = throw java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true

class Cons(val head: Tweet, val tail: TweetList) extends TweetList:
  def isEmpty = false


object GoogleVsApple:
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet =
    TweetReader.allTweets.filter(tweet => google.exists(y => tweet.text.contains(y)))

  lazy val appleTweets: TweetSet =
    TweetReader.allTweets.filter(tweet => apple.exists(y => tweet.text.contains(y)))

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList =
    googleTweets.union(appleTweets).descendingByRetweet

object Main extends App:
  // Print the trending tweets
  GoogleVsApple.trending foreach println
