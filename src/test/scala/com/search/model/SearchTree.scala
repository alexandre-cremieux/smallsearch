package com.search.model

import org.scalatest._

abstract class SearchTreeSpec extends FlatSpec
  with Matchers with OptionValues with Inside with Inspectors

class SearchTreeTest extends UnitSpec {

  var tree: SearchTree[StringItem] = SearchTree("Hello", 0) +
    Token("World,", 1) +
    Token("Best", 2) +
    Token("Brave", 3) +
    Token("World!", 4)

  "Tree" should "contain the added words" in {
    val contains: Token[StringItem] => Boolean = tree.contains((l, r) => l.compare(r))
    assert(contains(Token("Hello")))
    assert(contains(Token("World,")))
    assert(contains(Token("Best")))
    assert(contains(Token("Brave")))
    assert(contains(Token("World!")))
  }

  var otherTree: SearchTree[StringItem] = SearchTree("When", 5) +
    Token("The", 6) +
    Token("Night", List(7, 10)) +
    Token("Has", 8) +
    Token("Come", 9) ++ tree

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
      case SomeToken(item, _, _) => item.toUpperCase().startsWith("W")
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
      case SomeToken(item, _, _) =>
        if (item == token.get) (true, Token(local.get, 1, local.order))
        else (false, EmptyToken)
    })(Token("World!", 0, List()))
    result match {
      case SomeToken(item, score, _) =>
        assert(score(0) == 1.0)
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
      case (SomeToken(_, _, lO), SomeToken(_, _, rO)) => lO(0).compareTo(rO(0))
    }, t => t.flatOrders())
    assert(tokenInsertOrder.nonEmpty)
    tokenInsertOrder.iterator.reduce((l, r) => {
      assert(l.order.size == 1)
      assert(r.order.size == 1)
      assert(l.order(0) <= r.order(0))
      r
    })
    val orderedTokens = otherTree.collectByOrder()
    assert(orderedTokens.nonEmpty)
    val last = orderedTokens.iterator.reduce((l, r) => {
      assert(l.order.size == 1)
      assert(r.order.size == 1)
      assert(l.order(0) <= r.order(0))
      r
    })
    assert(last match {case SomeToken(i, _, _) => i == "Night"})
  }
}
