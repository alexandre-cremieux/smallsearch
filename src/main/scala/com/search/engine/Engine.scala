package com.search.engine

import com.search.model.{Document, EmptyToken, Leaf, SearchTree, SomeToken, StringItem, TInfo, Token}

import scala.collection.parallel.mutable.ParMap

/**
 * Content: The engine, the search result.
 *
 * NOTE: Please read the notes of the Engine class.
 */

/**
 * The main search engine object.
 *
 * Exposes the main operation available from the search engine:
 *    - indexing
 *    - exact match search
 *    - fuzzy search
 *
 * Note:
 *  *** The fact that this class is a singleton implies that the app is not suitable
 * for use by multiple users accessing separated accounts. In such a situation, the
 * engine should be instantiated as many time needed for user purpose or expose a new
 * implementation able to handle multiple connection.
 *
 *  *** This implementation is not suitable for indexing multiple heavy loaded documents.
 * To do so, it is necessary to distribute multiple instances, implement a sharding
 * mechanism and change the search tree for a balanced one (RBTree for in memory,
 * or BTree for disk). For indexing multiple document and avoid using multiple trees it
 * could be interesting to implement a tree where each token hold a reference to the
 * documents from which it has been indexed.
 *
 *  *** The fuzzy search may not be efficient when searching for a word missing the first
 * letter of the correct spelling counterpart. To avoid this issue, it may be useful
 * to implement a spelling corrector that would be queried before searching in the tree.
 *
 */
object Engine {
  private var tree: SearchTree[StringItem] = Leaf()

  /**
   * Index a document into a search tree.
   *
   * Note: This method could save the document on the disk if the document is already
   * defined into the docs map. Then the URI obtained could be stored into the map in a
   * tuple defined as (Document, List[URI])
   *
   * @param ref Reference of the document to create
   * @param items The items to add into the document
   */
  def index(ref: String, items: Iterator[String]): Unit = {
    val tokens = items.zipWithIndex.map({
      case (item, order) => Token(ref, item, order)
    }).toList
    val document = new Document(ref, tokens)
    this.tree = this.tree ++ document.tree
  }

  /**
   * Execute the search.
   * @param matcher The token matcher
   * @param items The items to search
   * @param sanitizer The item sanitizer.
   * @return The search result.
   */
  private
  def search(matcher: MatchSelect, items: List[String], sanitizer: String => String):
  SearchResult = {
    val tokens = items.zipWithIndex.map({
      case (item, order) =>
        val token = Token(sanitizer(item))
        Token(token.get, 0, order)
    })
    val searchResult = this.tree.searchToList(matcher.apply)(tokens)
    new SearchResult("", tokens, searchResult)
  }

  /**
   * Proceed an exact match against all the indexed documents.
   *
   * Note: Part of the code is repeated in the different search methods. This could
   * resolved by defining and implementing a trait that would take in argument a
   * function defining the search method to use (exact matching, fuzzy search...).
   *
   * @param items The items to search.
   * @param sanitizer to be applied on each item of the tree before proceeding to the
   *                  comparison between the indexed items and the items to search.
   * @return A scored SearchResult.
   */
  def exactMatch(items: List[String], sanitizer: String => String): SearchResult = {
    if (this.tree.isEmpty || items.isEmpty) Result.noDocumentError
    else {
      val matcher: MatchSelect = exactMatch(token => Token(sanitizer(token.get), List()))
      Engine.search(matcher, items, sanitizer)
    }
  }

  /**
   * Proceed an fuzzy match against all the indexed documents.
   * @param items The items to search.
   * @param sanitizer to be applied on each item of the tree before proceeding to the
   *                  comparison between the indexed items and the items to search.
   * @return A scored SearchResult.
   */
  def fuzzy(items: List[String], sanitizer: String => String): SearchResult = {
    if (this.tree.isEmpty || items.isEmpty) Result.noDocumentError
    else {
      val matcher: MatchSelect = fuzzyMatch(token => Token(sanitizer(token.get),List()))
      Engine.search(
        matcher,
        items.map(i => if (i.length <= 5) i else i.substring(0, 5)),
        sanitizer
      )
    }
  }

  /**
   * Define the exact match inner mechanism.
   * @param sanitizer The item sanitizer.
   * @param l The left item to search
   * @param r The item indexed by a tree.
   * @return a couple of (boolean, token) where the boolean indicates if the search
   *         should continue (false) or stop (true) and the token represents the item
   *         that corresponds to the match.
   */
  private def exactMatch
  (sanitizer: Token[StringItem] => Token[StringItem])
  (l: Token[StringItem], r: Token[StringItem]):
  (Boolean, Token[StringItem]) = {
    val sanitized = sanitizer(r)
    val isEqual = l == sanitized
    if (isEqual) (true, Token(r, 1.0))
    else (false, EmptyToken)
  }

  /**
   * Define the fuzzy search comparison inner mechanism.
   *
   * Based on the Levenshtein distance.
   *
   * Note: The algorithm can be quite slow with long tokens, it will not be suitable for
   * search on long string. The binary search tree model implies that some search
   * will not be correctly completed, especially when the first letter of a word is
   * missing and make the comparison fail by defining the route to parse within the graph
   * exposed by a binary search tree.
   *
   * @param sanitizer The item sanitizer.
   * @param l The left item to search
   * @param r The item indexed by a tree.
   * @return a couple of (boolean, token) where the boolean indicates if the search
   *         should continue (false) or stop (true) and the token represents the item
   *         with the best score..
   */
  private def fuzzyMatch
  (sanitizer: Token[StringItem] => Token[StringItem])
  (l: Token[StringItem], r: Token[StringItem]):
  (Boolean, Token[StringItem]) = {
    val sanitized = sanitizer(r)
    val distance = {
      if (l == sanitized) 0
      else {
        Token.levenshtein(
          l,
          if (sanitized.length <= 5) sanitized else sanitized.substring(0, 5)
        )
      }
    }
    if (distance == 0) (true, Token(r, 1))
    else l match {
      case EmptyToken => (false, EmptyToken)
      case SomeToken(item, info) =>
        val score = (item.get.length - distance) / item.get.length.toDouble
        if (score < info.head.score) {
          (false, l)
        } else {
          (false, Token(r.get, TInfo.addScore(r.info, score)))
        }
    }
  }
}

/**
 * Select the type of matcher to execute.
 */
trait MatchSelect {
  def apply(l: Token[StringItem], r: Token[StringItem]):(Boolean, Token[StringItem])
}

/**
 * The document name and the search tree resulting from a search of string.
 * @param document The document name.
 * @param found The resulting tree of a search.
 */
class DocResult(document: String, found: List[Token[StringItem]], score: Double) {

  def name: String = this.document

  def getScore: Double = this.score

  override def toString: String = {
    """
      |%s
      |%s
      |%s
      """.format(this.document, this.found, this.score)
  }
}

object DocResult {
  def apply
  (document: String, found: List[Token[StringItem]], searched: List[Token[StringItem]]):
  DocResult =
    new DocResult(
      document,
      found,
      DocResult.scoring(found, searched)
    )

  def scoring(found: List[Token[StringItem]], searched: List[Token[StringItem]]):
  Double = {
    val searchedSize = searched.size
    if (searchedSize != 0) {
      val tokenScore = {
        val rawScore = found.flatMap(t => t.info.map(i => i.score)).sum
        if (rawScore > searchedSize) searchedSize else rawScore
      }
      val ordered = {
        def comparator(l: Token[StringItem], r: Token[StringItem]): Int = (l, r) match {
          case (EmptyToken, EmptyToken) => 0
          case (SomeToken(_, _), EmptyToken) => -1
          case (EmptyToken, SomeToken(_, _)) => 1
          case (SomeToken(_, Nil), SomeToken(_, Nil)) => 0
          case (SomeToken(_, _), SomeToken(_, Nil)) => 1
          case (SomeToken(_, Nil), SomeToken(_, _)) => -1
          case (SomeToken(_, x :: _), SomeToken(_, y :: _)) => x.order.compareTo(y.order)
        }
        val fullList = Token
          .sort(comparator, found.flatMap(t => t.flatOrders()).toArray)
          .toList
        if (fullList.size > searchedSize) fullList.distinctBy(t => t.get) else fullList
      }
      val orderScore = searchedSize - Token.levenshteinList(searched, ordered)
      (tokenScore / searchedSize) * 0.75 + (orderScore / searchedSize) * 0.25
    } else {
      0
    }
  }
}

/**
 * A full search result obtained after executing a search against multiple documents.
 * @param error An error message if any.
 * @param searched The searched items
 * @param result The resulting tree.
 */
class SearchResult
(error: String, searched: List[Token[StringItem]], result: List[Token[StringItem]]) {

  def documents(): List[DocResult] = {
    this.result.flatMap(token => {
      token.info.map(info => (info.doc, Token(token.get, List(info))))
    }).groupBy(docToken => docToken._1)
      .map(entry => {
        val found = entry._2.map(e => e._2)
        DocResult(entry._1, found, searched)
      }).toList
      .sortBy(result => result.getScore)
      .reverse
  }

  def error(): Option[String] = this.error match {
    case "" => Option.empty
    case _ => Option(this.error)
  }

  override def toString: String =
    """
      |Searched: %s,
      |Error: %s,
      |Results: %s
      """.format(
        this.searched,
        this.error,
        this.result
    )
}

/**
 * Specific instructions links to the Result class.
 */
object Result {
  def noDocumentError: SearchResult = new SearchResult(
    "No indexed document",
    List.empty,
    List.empty
  )
}