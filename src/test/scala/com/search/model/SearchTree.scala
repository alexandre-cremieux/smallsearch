package com.search.model

import org.scalatest._

abstract class SearchTreeSpec extends FlatSpec
  with Matchers with OptionValues with Inside with Inspectors

class SearchTreeTest extends UnitSpec {

  val doc = ""
  val tree: SearchTree[StringItem] = SearchTree("Hello", 0) +
    Token(doc, "World,", 1) +
    Token(doc, "Best", 2) +
    Token(doc, "Brave", 3) +
    Token(doc, "World!", 4)

  "Tree" should "contain the added words" in {
    val contains: Token[StringItem] => Boolean = tree.contains((l, r) => l.compare(r))
    assert(contains(Token("Hello")))
    assert(contains(Token("World,")))
    assert(contains(Token("Best")))
    assert(contains(Token("Brave")))
    assert(contains(Token("World!")))
  }

  var otherTree: SearchTree[StringItem] = SearchTree("When", 5) +
    Token(doc, "The", 6) +
    Token("Night", List(TInfo(doc, 5), TInfo(doc, 5))) +
    Token(doc, "Has", 8) +
    Token(doc, "Come", 9) ++ tree

  "Other tree" should "contain other tree" in {
    val contains: Token[StringItem] => Boolean = otherTree.contains((l, r) => l.compare(r))
    assert(contains(Token("Hello")))
    assert(contains(Token("World,")))
    assert(contains(Token("Best")))
    assert(contains(Token("Brave")))
    assert(contains(Token("World!")))
    assert(contains(Token("When")))
    assert(contains(Token("The")))
    assert(contains(Token("Night")))
    assert(contains(Token("Has")))
    assert(contains(Token("Come")))
  }

  var lessTree: SearchTree[StringItem] = otherTree - Token("When")

  "Less tree" should "not contain" in {
    val contains: Token[StringItem] => Boolean = lessTree.contains((l, r) => l.compare(r))
    assert(contains(Token("Hello")))
    assert(contains(Token("World,")))
    assert(contains(Token("Best")))
    assert(contains(Token("Brave")))
    assert(contains(Token("World!")))
    assert(!contains(Token("When")))
    assert(contains(Token("The")))
    assert(contains(Token("Night")))
    assert(contains(Token("Has")))
    assert(contains(Token("Come")))
  }

  "Filtering only item that begin with W" should "contain World and When" in {
    val filter: Token[StringItem] => Boolean = {
      case EmptyToken => false
      case SomeToken(item, _) => item.toUpperCase().startsWith("W")
    }
    val filteredTree = otherTree.filter(filter)
    assert(filteredTree.contains(t => t.get.compare("World!")))
    assert(filteredTree.contains(t => t.get.compare("World,")))
    assert(filteredTree.contains(t => t.get.compare("When")))
    assert(!filteredTree.contains(t => t.get.compare("Come")))
  }

  "Search" should "find World!" in {
    val result = otherTree.search((token, local) => local match {
      case EmptyToken => (false, token)
      case SomeToken(item, _) =>
        if (item == token.get) (true, Token(local, 1.0))
        else (false, EmptyToken)
    })(Token("World!", List()))
    result match {
      case SomeToken(_, Nil) => throw IllegalStateException
      case SomeToken(item, info) =>
        assert(info.head.score == 1.0)
        assert(item == "World!")
    }
  }

  "Sorting" should "give a correct order" in {
    val result = otherTree.sort((l, r) => l.compare(r), t => List(t))
    assert(result.nonEmpty)
    result.iterator.reduce((l, r) => {
      assert(l <= r)
      r
    })
    val tokenInsertOrder = otherTree.sort((l, r) => (l, r) match {
      case (SomeToken(_, lO), SomeToken(_, rO)) => lO.head.order.compareTo(rO.head.order)
    }, t => t.flatOrders())
    assert(tokenInsertOrder.nonEmpty)
    val last = tokenInsertOrder.iterator.reduce((l, r) => {
      assert(l.info.size == 1)
      assert(r.info.size == 1)
      assert(l.info.head.order <= r.info.head.order)
      r
    })
    val orderedTokens = otherTree.collectByOrder()
    assert(orderedTokens.nonEmpty)
    assert(last match {case SomeToken(i, _) => i == "Night"})
  }
}
