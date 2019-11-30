package com.search.model

import scala.collection.parallel.ParSeq
import scala.collection.mutable

import com.search.model.StringItem.StringItem2String

case class TInfo(doc: String, order: Int, weight: Double, score: Double)

/**
 * Token info object.
 */
object TInfo {
  def addScore(info: TInfo, s: Double): TInfo = TInfo(
    info.doc, info.order, info.weight, info.score
  )
  def addScore(info: List[TInfo], s: Double): List[TInfo] =
    info.map(i => TInfo.addScore(i, s))
  def empty: TInfo = TInfo("", 0, 0, 0)
  def apply(order: Int): TInfo = TInfo("", order, 0, 0)
  def apply(doc: String, order: Int): TInfo = TInfo(doc, order, 0, 0)
  def score(info: List[TInfo]): Double = info.map(i => i.score).sum
  def compareScore(l: List[TInfo], r: List[TInfo]): Int = score(l).compareTo(score(r))
}

/**
 * Represents an item wrapper. The implementation tries to stay close to the Option monad.
 *
 * The token class is designed in order to ease the addition of comparison operations
 * between the tokens composing a search tree and/or the term that should be searched by
 * the tree. In addition this wrapper should help to add more expressiveness to
 * a simple token like a weight definer or add more feature like a token sanitizer.
 *
 * The map and flatMap functions are defined to provide monadic operations to the
 * class in order to ensure a functional approach to the hierarchy. A unit method is
 * provided by the Token object.
 *
 * Compare to an Option class, the Token class offers a way to embed a Comparable
 * element to make ordering possible with pure Java object like String. As Option,
 * this hierarchy is designed to provide a way to deal with empty values without having
 * to hassle with issues induced by the null value.
 *
 * @tparam A The type of the item to wrap.
 * @see [[scala.Option]]
 */
sealed trait Token[+A] {

  /**Check if the token is empty*/
  def isEmpty: Boolean

  /**Return the inner item of the token*/
  def get: A

  def info: List[TInfo]

  /**Lesser or equal than comparison**/
  def <=[B >: A](that: Token[B])(implicit ev$1: B => Ordered[B]): Boolean =
    this.compare(that) <= 0

  /**Lesser than comparison*/
  def <[B >: A](that: Token[B])(implicit ev$1: B => Ordered[B]): Boolean =
    this.compare(that) < 0

  /**Greater or equal than comparison*/
  def >=[B >: A](that: Token[B])(implicit ev$1: B => Ordered[B]): Boolean =
    this.compare(that) >= 0

  /**Greater than comparison*/
  def >[B >: A](that: Token[B])(implicit ev$1: B => Ordered[B]): Boolean =
    this.compare(that) > 0

  /**
   * Merge the info of two tokens together into a new token with equal item.
   * This method is equal to <code>SomeToken(item, this.info ++ that.info)
   * @param that The token to merge.
   * @tparam B The inner type of the token.
   * @return A new token with same item and merged document.
   */
  def ++[B >: A](that: Token[B])(implicit ev$1: B => Ordered[B]): Token[B] =
    this.merge(that)

  /**Returns the result of applying f to this if non empty.*/
  @inline final def map[B <: Ordered[B]](f: (A, List[TInfo]) => (B, List[TInfo])):
  Token[B] =
    if (this.isEmpty) {
      EmptyToken
    } else {
      val (t, i) = f(this.get, this.info)
      SomeToken[B](t, i)
    }

  /**Returns the result of applying f to this if non empty.*/
  @inline final def flatMap[B <: Ordered[B]](f: (A, List[TInfo]) => Token[B]):
  Token[B] =
    if (this.isEmpty) {
      EmptyToken
    } else {
      f(this.get, this.info)
    }

  def compare[B >: A](that: Token[B])(implicit ev$1: B => Ordered[B]):
  Int = {
    this.get.compareTo(that.get)
  }

  /**
   * Flat out all the orders held by the token to a list of token holding only one order.
   * @return A list of tokens holding only one order.
   */
  def flatOrders(): List[Token[A]] = List()

  def merge[B >: A](that: Token[B])(implicit ev$1: B => Ordered[B]): Token[B] =
    if (this.isEmpty) that
    else {
      that match {
        case EmptyToken => this
        case SomeToken(i, info) => if (i != this.get) {
          throw new IllegalArgumentException("Item are not equals")
        } else {
          val nInfo = this.info ++ info
          SomeToken(this.get, nInfo)
        }
      }
    }

  /**
   * Merge by adding a score to each of the document info of each of the two token
   */
  def merge[B >: A](that: Token[B], score: Double)(implicit ev$1: B => Ordered[B]):
  Token[B] = this match {
    case EmptyToken =>
      if (that.isEmpty) Token.empty
      else SomeToken(that.get, TInfo.addScore(that.info, score))
    case SomeToken(t, i) =>
      if (that.isEmpty) {
        SomeToken(t, TInfo.addScore(that.info, score))
      } else {
        SomeToken(t, i.map(d => TInfo.addScore(d, score))) ++
          SomeToken(t, TInfo.addScore(that.info, score))
      }
  }
}

/**
 * This case class represents an non-existent value.
 *
 * The EmptyToken cannot be a class due to the fact the Token should embed a Comparable
 * object. Comparable type cannot be covariant and this prevent from comparing a specific
 * type to Nothing (T >: Nothing). This is resulting from the choice made by Scala to
 * ensure compatibility with Java String which is the format understood by the JVM.
 *
 * It would not have been possible to use the Ordered trait instead because the String are
 * not compatible with this pure Scala type.
 */
case object EmptyToken extends Token[Nothing] {

  override def isEmpty: Boolean = true

  override def get: Nothing = throw new NoSuchElementException("EmptyToken.get")

  override def info: List[TInfo] = throw new NoSuchElementException("EmptyToken.get")

  /**Return "Empty Token"*/
  override def toString: String = "EmptyToken"

  override def compare[B >: Nothing]
  (that: Token[B])(implicit ev$1: B => Ordered[B]): Int = that match {
    case EmptyToken => 0
    case _ => throw new IllegalArgumentException
  }
}

/**Represents a non empty token of type A.
 *
 * The SomeToken class provides a order parameter that should help the calling code to
 * represent a whole document. This could be especially useful when using Tokens to
 * index item of a document with a binary search tree and provide a mechanism to
 * rebuild the original document without having to save it in memory.
 *
 * @param item The inner value of the token
 * @param info The info of the token
 * @tparam A The type of the inner token
 */
case class SomeToken[A <: Ordered[A]](item: A, info: List[TInfo]) extends Token[A] {

  override def get: A = this.item

  override def isEmpty: Boolean = false

  override def equals(that: Any): Boolean = that match {
    case EmptyToken => false
    case SomeToken(item, _) => item == this.item
    case _ => this.get == that
  }

  override def toString: String = {
    "SomeToken(" + String.valueOf(item) + ", " + this.info + ")"
  }

  def compareTo(that: Token[A]): Int = that match {
    case SomeToken(item, _) => this.item.compareTo(item)
    case EmptyToken => throw new IllegalArgumentException("EmptyToken")
  }

  override def flatOrders(): List[Token[A]] = this.info.iterator.map(i => {
    SomeToken(item, List(i))
  }).toList
}

/**Utilities object for the token class*/
object Token {

  import scala.language.implicitConversions

  def apply[A <: Ordered[A]](item: A): Token[A] = new SomeToken[A](item, Nil)

  def apply[A <: Ordered[A]](token: Token[A], score: Double): Token[A] = token match {
    case EmptyToken => EmptyToken
    case SomeToken(item, info) => new SomeToken[A](item, TInfo.addScore(info, score))
  }

  def apply[A <: Ordered[A]](item: A, score: Double, order: Int):
  Token[A] = {
    item match {
      case "" => Token.empty
      case _ => new SomeToken[A](item, List(TInfo("", order, 0, score)))
    }
  }

  def apply(item: String): Token[StringItem] = item match {
    case "" => Token.empty
    case _ => new SomeToken[StringItem](new StringItem(item), Nil)
  }

  def apply(item: String, doc: String, order: Int): Token[StringItem] = item match {
    case "" => Token.empty
    case _ => new SomeToken[StringItem](StringItem(item), List(TInfo(doc, order)))
  }

  /**
   * Instantiate a String parameterized Token.
   *
   * If the String is empty then an empty token is returned.
   * @param item The String to wrap.
   * @param order The
   * @return
   */
  @deprecated
  def apply(item: String, order: Int): Token[StringItem] = item match {
    case "" => Token.empty
    case _ => new SomeToken[StringItem](new StringItem(item), List(TInfo(order)))
  }

  def apply(item: String, info: List[TInfo]): Token[StringItem] = item match {
    case "" => Token.empty
    case _ => new SomeToken[StringItem](new StringItem(item), info)
  }

  def apply[A <: Ordered[A]](item: A, info: List[TInfo]): Token[A] = item match {
    case "" => Token.empty
    case _ => new SomeToken[A](item, info)
  }

  /**
   * Return an empty token of type A
   * @tparam A The awaited type.
   * @return An empty token.
   */
  def empty[A <: Ordered[A]]: Token[A] = EmptyToken

  /**Implicit conversion of a String to a Token of type String.
   *
   * By default, the order of the newly created token is set to 0.
   * @param item The item to convert
   * @return An EmptyToken of type String if item is empty (equal to "") or a SomeToken
   *         of type A wrapping if the item is a full String.
   */
  implicit def string2Token(item: String): Token[StringItem] = Token(new StringItem(item))

  /**
   * Implicit converter from string to token.
   * @param item The string to convert to a token.
   * @return A StringItem token.
   */
  implicit def token2String(item: Token[StringItem]): String = item match {
    case EmptyToken => ""
    case SomeToken(item, _) => item.get
  }

  /**
   * Quick sort of an array of tokens.
   * @param comp The comparator used to compare two tokens.
   * @param xs The array to sort.
   * @tparam A The type of the tokens to compare.
   * @return A sorted list of tokens.
   */
  def sort[A <: Ordered[A]](comp: (Token[A], Token[A]) => Int, xs: Array[Token[A]]):
  Array[Token[A]] = {
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)
      val comparator = (t: Token[A]) => comp(pivot, t)
      ParSeq(
        sort(comp, xs.filter(t => comparator(t) > 0)),
        xs filter (t => comparator(t) == 0),
        sort(comp, xs.filter(t => comparator(t) < 0))).reduce(_ ++ _)
    }
  }

  /**
   * Compute a Levenshtein distance between to String parameterized Token.
   *
   * The method assumes that the left Token is the standard and that it is not empty. The
   * right Token is the one to be test.
   *
   * This implementation is based on the proposed by Wikibooks in the functional style:
   * https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance
   * @param lToken The lToken (the standard)
   * @param rToken The rToken to test.
   * @return The Levenshtein distance between l and r.
   * @throws IllegalArgumentException If lToken is empty
   */
  def levenshtein(lToken: Token[StringItem], rToken: Token[StringItem]): Int = {
    if (lToken.isEmpty) {
      throw new IllegalArgumentException("Empty token")
    } else if (rToken.isEmpty) {
      return lToken.get.length
    }

    val memorizedCosts = mutable.Map[(Int, Int), Int]()
    val lString = lToken.get.get
    val rString = rToken.get.get

    def lev: ((Int, Int)) => Int = {
      case (k1, k2) => memorizedCosts.getOrElse((k1, k2), (k1, k2) match {
        case (i, 0) => i
        case (0, j) => j
        case (i, j) =>
          ParSeq(
            1 + lev((i - 1, j)),
            1 + lev((i, j - 1)),
            lev((i - 1, j - 1)) + (if (lString(i - 1) != rString(j - 1)) 1 else 0)).min
      })
    }
    lev((lString.length(), rString.length()))
  }

  /**
   * Compute a Levenshtein distance on two lists of tokens.
   *
   * The comparison is based on exact match.
   *
   * @param lToken The main list. If empty then an IllegalArgumentException is thrown.
   * @param rToken The right list against which the distance is computed.
   * @return The Levenshtein distance resulting from comparing the two lists.
   */
  def levenshteinList(lToken: List[Token[StringItem]], rToken: List[Token[StringItem]]):
  Int = {
    if (lToken.isEmpty) {
      throw new IllegalArgumentException("Empty token")
    } else if (rToken.isEmpty) {
      return lToken.size
    }

    val memorizedCosts = mutable.Map[(Int, Int), Int]()
    val lString = lToken.map(a => a.get)
    val rString = rToken.map(a => a.get)

    def lev: ((Int, Int)) => Int = {
      case (k1, k2) => memorizedCosts.getOrElse((k1, k2), (k1, k2) match {
        case (i, 0) => i
        case (0, j) => j
        case (i, j) =>
          ParSeq(
            1 + lev((i - 1, j)),
            1 + lev((i, j - 1)),
            lev((i - 1, j - 1)) + (if (lString(i - 1) != rString(j - 1)) 1 else 0)).min
      })
    }
    lev((lString.size, rString.size))
  }
}
