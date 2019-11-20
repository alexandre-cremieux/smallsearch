package com.search.engine

import com.search.model.{EmptyToken, Document, SearchTree, SomeToken, StringItem, Token}

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
  private val docs = ParMap[String, Document]()

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
    val doc = new Document(ref)
    var i = 0
    while (items.hasNext) {
      doc.tree += Token(items.next(), i)
      i += 1
    }
    docs.put(ref, doc)
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
    val idx = for(i <- 0 until items.size) yield i
    val tokens = items.zip(idx).map({
      case (item, order) =>
        val token = Token(sanitizer(item))
        Token(token.get, 0, order)
    })
    val resultTrees = docs.values.par
      .map(doc => {
        val result = new DocResult(
          doc.name(),
          doc.tree.searchToTree(matcher.apply)(tokens)
        )
        result.scoring(tokens)
        result
      }).toList
    new SearchResult("", tokens, resultTrees)
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
    if (this.docs.isEmpty || items.isEmpty) Result.noDocumentError
    else {
      val matcher: MatchSelect = exactMatch(
        token => Token(sanitizer(token.get),List()))
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
    if (this.docs.isEmpty || items.isEmpty) Result.noDocumentError
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
  private def exactMatch(sanitizer: Token[StringItem] => Token[StringItem])
                        (l: Token[StringItem], r: Token[StringItem]): (Boolean, Token[StringItem]) = {
    if (l != sanitizer(r)) (false, EmptyToken)
    else (true, Token(l.get, 1, r.order))
  }

  /**
   * Define the fuzzy search comparison inner mechanism.
   *
   * Based on the Levenshtein distance.
   *
   * Note: The algorithm can be quite slow with long tokens, it will not be suitable for
   * search on long string. The binary search tree model implies that some search
   * will not be correctly complete, especially when the first letter of a word is
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
  private def fuzzyMatch(sanitizer: Token[StringItem] => Token[StringItem])
                        (l: Token[StringItem], r: Token[StringItem]): (Boolean, Token[StringItem]) = {
    val sanitized = sanitizer(r)
    val distance = {
      if (l == sanitized) 0
      else {
        Token.levenshtein(l,
          if (sanitized.length <= 5) sanitized else sanitized.substring(0, 5)
        )
      }
    }
    if (distance == 0) (true, Token(r.get, 1, r.order))
    else l match {
      case SomeToken(item, scores, _) =>
        val score = (item.item.length - distance) / item.item.length.toDouble
        if (score < scores(0)) {
          (false, Token(r.get, scores, r.order))
        } else {
          (score > 0.8, Token(r.get, List(score), r.order))
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
class DocResult(document: String, found: SearchTree[StringItem]) {
  private var score: Double = -1

  def name: String = this.document

  def getScore: Double = this.score

  def found(): SearchTree[StringItem] = this.found

  /**
   * Score a document against a search result.
   * @param searched The searche
   * */
  def scoring(searched: List[Token[StringItem]]): Unit = {
    if (this.score == -1) {
      val tokenScore = found.score
      if (tokenScore == 0) {
        this.score = 0
      } else {
        val searchedSize = searched.size
        val ordered = this.found.collectByOrder()
        val ordSize = ordered.size
        val orderScore = ordSize -
          Token.levenshteinList(searched, ordered) +
          Math.abs(ordSize - searchedSize)
        this.score =
          (tokenScore / searchedSize) * 0.75 +
            (orderScore / ordSize) * 0.25
      }
    }
  }

  override def toString: String = {
    """
      |%s
      |%s
      |%s
      """.format(this.document, this.found, this.score)
  }
}

/**
 * A full search result obtained after executing a search against multiple documents.
 * @param error An error message if any.
 * @param searched The searched items
 * @param results The resulting tree.
 */
class SearchResult
(error: String, searched: List[Token[StringItem]], results:List[DocResult]) {
  def documents(): List[DocResult] = this.results.sortBy(r => -r.getScore)
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
      this.results.sortBy(r => -r.getScore))
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