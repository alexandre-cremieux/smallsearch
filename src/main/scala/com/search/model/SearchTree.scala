package com.search.model

import scala.language.implicitConversions
import scala.collection.parallel.ParSeq

/**
 * Represent a set of [[Token]] of type A designed as a binary search
 * tree. Each branch of a tree has two children. The search tree maintains an invariant:
 * for each branch, all the Tokens in the left subtree are smaller than the Token in the
 * actual branch. Each Tokens in the right subtree are larger.
 *
 * The advantage of presenting the search tree as a set is that the complexity of a
 * search operation is 0(log n) where n is the number of Token held by the tree. However,
 * this Tree is not balanced and this may cause indexing error, especially if the string
 * is close to the bounds of the alphabet ordering ranges.
 *
 * NOTE: Tree is not balanced. This means that a search may not be in 0(lg n) if all
 * added token are greater or lesser order than the first token inserted. To avoid this
 * state and if the search tree should be kept in RAM then a red - black tree would be
 * more suitable.
 *
 * It would have been possible to use the Scala mutable TreeSet to perform such task.
 * However, implementing a specific data structure will ease any further dedicated
 * development of the app.
 *
 * @tparam A The raw type of the items held the tree
 * @see [[Token]]
 */
trait SearchTree[A <: Ordered[A]] {

  /**Return true if the tree is empty and false otherwise*/
  def isEmpty: Boolean

  def score: Double = this match {
    case Leaf() => 0
    case SomeSearchTree(item, l, r) => ParSeq(
      item.info.iterator.map(i => i.score).sum,
      l.score,
      r.score
    ).sum
  }

  /**
   * Add a new token to be indexed by the tree.
   *
   * If the token is empty then nothing happens and the current tree is returned.
   * Otherwise a new SearchTree containing the new token and all the tokens of this set
   * is returned.
   *
   * If this contains the token to add then the current set is returned.
   *
   * @param token The Token to add.
   * @return A new SearchTree containing the newly added Token
   */
  def +(token: Token[A]): SearchTree[A]

  /**
   * Build and return a new TreeSet representing the union of this and that.
   * @param that The TreeSet to be merge with this.
   * @return A new SearchTree of type B resulting of the union of this and that.
   */
  def ++(that: SearchTree[A]): SearchTree[A] = {
    if (this.isEmpty) that
    else if (that.isEmpty) this
    else this.filterAcc(_ => true, that)
  }

  /**
   * Remove a token from this SearchTree.
   *
   * If the token is not contained by this or if the token is empty then this is returned
   * otherwise a new SearchTree without the token is returned.
   *
   * @param token The token to remove.
   * @return A new SearchTree parameterized by B.
   */
  def -(token: Token[A]): SearchTree[A]

  /**
   * Test if the search tree contains a specific token.
   * @param p The comparison function. Should return 0 if the indexed token corresponds
   *          to the search item.
   * @return True if one of the indexed token corresponds to the searched token.
   */
  def contains(p: Token[A] => Int): Boolean

  /**
   * Take a comparing function that compare the Tokens of the tree and an initial one.
   *
   * Return true if one the result is equal to 0.
   *
   * This method can be useful if the calling code needs to apply a function to a Token
   * before comparing it to the initial one (case insensitive search, etc.).
   *
   * The currying approach should help the calling code to proceed a specific test while
   * iterating over a list of Token.
   * @param comparator The predicate
   * @param token The token to search.
   * @return True if one of the token is
   */
  def contains(comparator: (Token[A], Token[A]) => Int)(token: Token[A]): Boolean

  /**
   * Take a predicate and return a subset of the SearchTree containing all the tokens
   * for which the predicate is true.
   * @param p The filter predicate.
   * @return A subset of parameter B of the SearchTree for which the predicate is true.
   */
  def filter(p: Token[A] => Boolean): SearchTree[A] = {
    filterAcc(p, Leaf())
  }

  /**
   * Propagation method of 'filter'.
   * @param p The filter predicate.
   * @param acc The accumulated SearchTree.
   * @return A subset of parameter B of the SearchTree for which the predicate is true.
   * @see [[SearchTree.filter]]
   */
  def filterAcc(p: Token[A] => Boolean, acc: SearchTree[A]): SearchTree[A]

  /**
   * Search a Token by executing a scoring function the token contained in the tree and
   * an initial Token.
   *
   * The selection should return a pair of (Boolean, Score, Token) where the boolean
   * indicate to the method if it should stop or continue to search another token in the
   * tree (true for stop, false for continue).
   *
   * The score is a Double that should be between 0 and 1 and the initial score should
   * be 0.0.
   *
   * The currying approach should help the calling code to define a simple selection
   * function.
   *
   * The method should be especially useful to proceed "fuzzy" search among the token
   * of the tree while iterating though a list of Token to score.
   *
   * @param f The selection process
   * @param init The initial token and score against which the selection is executed.
   * @return The selected Token and the computed score or the initial couple if the tree
   *         is empty.
   */
  def search(f:(Token[A], Token[A]) => (Boolean, Token[A]))(init: Token[A]): Token[A]

  /**
   * Search a list of tokens from within the tree.
   *
   * The method uses the ParSeq method to parallelize the search before building the
   * output search tree.
   *
   * Note: A solution should be found to limit the number of thread executing a search
   * in order to not overload the system.
   *
   * @param f The comparison method to be used during the search.
   * @param tokens The tokens to search by the tree.
   * @return A new search tree (to be seen as a set) containing the item found within
   *         the tree.
   */
  def searchToTree
  (f:(Token[A], Token[A]) => (Boolean, Token[A]))(tokens: List[Token[A]]):
  SearchTree[A] = {
    if (this.isEmpty) new Leaf[A]
    else {
      val search: Token[A] => Token[A] = this.search(f)
      ParSeq(
        tokens.map(search)
          .filter(t => !t.isEmpty)
          .map(t => SearchTree(t))
      ).reduce(_ ++ _).reduceLeftOption(_ ++ _).getOrElse(Leaf[A]())
    }
  }

  /**
   * Search a list of tokens from within the tree.
   *
   * The method uses the ParSeq method to parallelize the search before building the
   * output search tree.
   *
   * Note: A solution should be found to limit the number of thread executing a search
   * in order to not overload the system.
   *
   * @param f The comparison method to be used during the search.
   * @param tokens The tokens to search by the tree.
   * @return A new list containing the item found within the tree.
   */
  def searchToList
  (f:(Token[A], Token[A]) => (Boolean, Token[A]))(tokens: List[Token[A]]):
  List[Token[A]] = {
    if (this.isEmpty) List.empty
    else {
      if (tokens.size <= 2) {
        val search: Token[A] => Token[A] = this.search(f)
        tokens.map(search)
          .filter(t => !t.isEmpty)
      } else {
        val lists = tokens.splitAt(tokens.size / 2)
        ParSeq(
          this.searchToList(f)(lists._1),
          this.searchToList(f)(lists._2)
        ).reduce(_ ++ _)
      }
    }
  }

  /**
   * Collect all the items of the tree into a list. This method could be seen as a map
   * method.
   *
   * @param f Expander applied on each token.
   * @return A List containing all the token in the tree.
   */
  def collect(f: Token[A] => List[Token[A]]): List[Token[A]] = {
    if (this.isEmpty) List()
    else {
      this match {
        case Leaf() => Nil
        case SomeSearchTree(token, left, right) => ParSeq(
          f(token),
          left.collect(f),
          right.collect(f)
        ).reduce(_ ++ _)
      }
    }
  }

  /**
   * Collect all the items of the tree into a list. This method could be seen as a map
   * method.
   *
   * The method sort all the items collected by order before returning the result.
   *
   * @return A list of indexed item sorted by order.
   */
  def collectByOrder(): List[Token[A]] = {
    def comparator(l: Token[A], r: Token[A]): Int = (l, r) match {
      case (SomeToken(_, Nil), SomeToken(_, Nil)) => 0
      case (SomeToken(_, _), SomeToken(_, Nil)) => 1
      case (SomeToken(_, Nil), SomeToken(_, _)) => -1
      case (SomeToken(_, x :: _), SomeToken(_, y :: _)) => x.order.compareTo(y.order)
    }
    this.sort(comparator, token => token.flatOrders())
  }

  /**
   * Sort the item indexed by the tree with a specific comparison function.
   * @param f comparison function
   * @param m mapping function for each token.
   * @return The indexed item sorted in order based on the comparison function.
   */
  def sort(f: (Token[A], Token[A]) => Int, m: Token[A] => List[Token[A]]):
  List[Token[A]] = {
    if (this.isEmpty) List.empty
    else {
      Token.sort(f, this.collect(m).toArray).toList
    }
  }
}

/**
 * Represent a leaf in the tree.
 *
 * Note: There is a problem here with this extension of the SearchTree hierarchy. This
 * class should be an object. But the implementation of generic methods here are not
 * accepted by the compiler if declared as an object parameterized to Nothing (Disrespect
 * of the contravariant type checking of Scala). To solve this,each method should be
 * implemented in the SearchTree class.
 *
 * @tparam A The raw type of the items held the tree
 */
case class Leaf[A <: Ordered[A]]() extends SearchTree[A] {

  override def isEmpty: Boolean = true

  override def +(token: Token[A]): SearchTree[A] = token match {
    case token: SomeToken[A] => SearchTree(token, this, this)
    case _ => Leaf()
  }

  override def -(token: Token[A]): SearchTree[A] = this

  override def search
  (f: (Token[A], Token[A]) => (Boolean, Token[A]))(init: Token[A]):
  Token[A] = {
    EmptyToken
  }

  override def contains(p: Token[A] => Int): Boolean = false

  override def contains
  (comparator: (Token[A], Token[A]) => Int)(token: Token[A]): Boolean = {
    false
  }

  override def filterAcc(p: Token[A] => Boolean, acc: SearchTree[A]): SearchTree[A] = {
    acc
  }
}

/**
 * Represents an active tree. A tree is basically a node holding a token and pointing to
 * the left and right roots of the tree.
 * @param token The holded token.
 * @param left The left tree (leaf or search tree)
 * @param right The right tree (leaf or search tree)
 * @tparam A The raw type of the items held the tree
 */
case class SomeSearchTree[A <: Ordered[A]]
(token: Token[A], left: SearchTree[A], right: SearchTree[A])
extends SearchTree[A] {

  override def isEmpty: Boolean = false

  override def +(token: Token[A]): SearchTree[A] = token match {
    case EmptyToken => this
    case token: Token[A] => if (token < this.token) {
      SearchTree(this.token, left + token, right)
    } else if (token > this.token) {
      SearchTree(this.token, left, right + token)
    } else {
      SearchTree(this.token ++ token, left, right)
    }
  }

  override def -(token: Token[A]): SearchTree[A] = token match {
    case EmptyToken => this
    case _ => if (token < this.token) {
      SomeSearchTree(this.token, this.left - token, this.right)
    } else if (token > this.token) {
      SomeSearchTree(this.token, this.left, this.right - token)
    } else {
      this.left ++ this.right
    }
  }

  def contains(p: Token[A] => Int): Boolean = {
    val result = p(this.token)
    if (result > 0) this.left.contains(p)
    else if (result < 0) this.right.contains(p)
    else true
  }

  def contains(comparator: (Token[A], Token[A]) => Int)(token: Token[A]): Boolean = {
    val p: Token[A] => Int = local => comparator(local, token)
    this.contains(p)
  }

  override def filterAcc(p: Token[A] => Boolean, acc: SearchTree[A]): SearchTree[A] = {
    if (p(this.token)){
      left.filterAcc(p, this.right.filterAcc(p, acc + this.token))
    } else {
      left.filterAcc(p, this.right.filterAcc(p, acc))
    }
  }

  override def search
  (scoring: (Token[A], Token[A]) => (Boolean, Token[A]))(init: Token[A]): Token[A] = {
    val localResult = scoring(init, this.token)
    if (localResult._1) localResult._2
    else {
      val lesser = this.token.get.toString.toUpperCase() > init.get.toString.toUpperCase()
      val subResult = if (!this.left.isEmpty && lesser) {
        this.left.search(scoring)(init)
      } else if (!this.right.isEmpty) {
        this.right.search(scoring)(init)
      } else {
        EmptyToken
      }

      val lResult = localResult._2
      (lResult, subResult) match {
        case (EmptyToken, EmptyToken) => EmptyToken
        case (EmptyToken, _) => subResult
        case (_, EmptyToken) => lResult
        case _ =>
          if (TInfo.compareScore(subResult.info, lResult.info) > 0) {
            subResult
          } else {
            lResult
          }
      }
    }
  }
}

/**
 * Utility object for the search tree.
 *
 * Exposes the unit method (apply) for the SearchTree hierarchy.
 */
object SearchTree {
  def apply[A <: Ordered[A]](token: Token[A]):SearchTree[A] =
    if (token.isEmpty) Leaf()
    else SomeSearchTree(token, Leaf(), Leaf())

  def apply[A <: Ordered[A]]
  (token: Token[A], l: SearchTree[A], r: SearchTree[A]): SearchTree[A] =
    if (token.isEmpty) l ++ r
    else SomeSearchTree(token, l, r)

  def apply(item: String): SearchTree[StringItem] =
    new SomeSearchTree[StringItem](
      Token(item), Leaf(), Leaf()
    )

  def apply(item: String, order: Int): SearchTree[StringItem] =
    new SomeSearchTree[StringItem](
      Token(item, order), Leaf(), Leaf()
    )
}
