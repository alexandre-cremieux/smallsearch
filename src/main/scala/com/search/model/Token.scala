package com.search.model

import scala.collection.parallel.ParSeq
import scala.collection.mutable

import com.search.model.StringItem.StringItem2String

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

  /**Return the inner item of the token*/
  def get: A

  def scores: List[Double]

  /**Return the order of the Token */
  def order: List[Int]

  /**Return true does not embed any item*/
  def isEmpty: Boolean

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

  /**Returns the result of applying f to this if non empty.*/
  @inline final def map[B <: Ordered[B]]
  (f: (A, List[Double], List[Int]) => (B, List[Double], List[Int])): Token[B] =
    if (this.isEmpty) {
      EmptyToken
    } else {
      val result = f(this.get, this.scores, this.order)
      SomeToken[B](result._1, result._2, result._3)
    }

  /**Returns the result of applying f to this if non empty.*/
  @inline final def flatMap[B <: Ordered[B]](f: (A, List[Int]) => Token[B]): Token[B] =
    if (this.isEmpty) {
      EmptyToken
    } else {
      f(this.get, this.order)
    }

  def compare[B >: A](that: Token[B])(implicit ev$1: B => Ordered[B]): Int = {
    this.get.compareTo(that.get)
  }

  /**
   * Flat out all the orders held by the token to a list of token holding only one order.
   * @return A list of tokens holding only one order.
   */
  def flatOrders(): List[Token[A]] = List()
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

  override def get: Nothing = throw new NoSuchElementException("EmptyToken.get")

  override def order: List[Int] = throw new NoSuchElementException("EmptyToken.get")

  override def isEmpty: Boolean = true

  override def scores: List[Double] = List.empty

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
 * @param orders The order of the token
 * @tparam A The type of the inner token
 */
case class SomeToken[A <: Ordered[A]](item: A, scores: List[Double], orders: List[Int])
  extends Token[A] {

  override def get: A = this.item

  override def isEmpty: Boolean = false

  override def order: List[Int] = orders

  override def equals(that: Any): Boolean = that match {
    case EmptyToken => false
    case SomeToken(item, _, _) => item == this.item
    case _ => this.get == that
  }

  override def toString: String = {
    "SomeToken(" + String.valueOf(item) + ", " + scores + ", " + order + ")"
  }

  def compareTo(that: Token[A]): Int = that match {
    case SomeToken(item, _, _) => this.item.compareTo(item)
    case EmptyToken => throw new IllegalArgumentException("EmptyToken")
  }

  override def flatOrders(): List[Token[A]] = this.orders match {
    case Nil => Nil
    case _ => orders.iterator.map(order => SomeToken(item, scores, List(order))).toList
  }
}

/**Utilities object for the token class*/
object Token {

  import scala.language.implicitConversions

  def apply[A <: Ordered[A]](item: A): Token[A] = new SomeToken[A](item, List(1), List())

  def apply[A <: Ordered[A]](item: A, order: Int): Token[A] =
    new SomeToken[A](item, List(1), List(order))
  /**
   * Instantiate a String parameterized Token.
   *
   * If the String is empty then an empty token is returned.
   * @param item The String to wrap.
   * @param order The
   * @return
   */
  def apply(item: String, order: Int): Token[StringItem] = item match {
    case "" => Token.empty
    case _ => new SomeToken[StringItem](new StringItem(item), List(1), List(order))
  }

  def apply(item: String, score: Double, orders: List[Int]):
  Token[StringItem] = item match {
    case "" => Token.empty
    case _ => new SomeToken[StringItem](new StringItem(item), List(score), orders)
  }

  def apply(item: String, orders: List[Int]): Token[StringItem] = item match {
    case "" => Token.empty
    case _ => new SomeToken[StringItem](new StringItem(item), List(1), orders)
  }

  def apply(item: String): Token[StringItem] = item match {
    case "" => Token.empty
    case _ => new SomeToken[StringItem](new StringItem(item), List(1), List())
  }

  def apply[A <: Ordered[A]](item: A, orders: List[Int]): Token[A] = item match {
    case "" => Token.empty
    case _ => new SomeToken[A](item, List(1), orders)
  }

  def apply[A <: Ordered[A]](item: A, score: Double, orders: List[Int]): Token[A] = {
    item match {
      case "" => Token.empty
      case _ => new SomeToken[A](item, List(score), orders)
    }
  }

  def apply[A <: Ordered[A]](item: A, scores: List[Double], orders: List[Int]):
  Token[A] = {
    item match {
      case "" => Token.empty
      case _ => new SomeToken[A](item, scores, orders)
    }
  }

  def apply[A <: Ordered[A]](item: A, score: Double, order: Int):
  Token[A] = {
    item match {
      case "" => Token.empty
      case _ => new SomeToken[A](item, List(score), List(order))
    }
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
    case SomeToken(item, _, _) => item.get
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
