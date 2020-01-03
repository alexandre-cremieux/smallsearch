package com.search.model

import org.scalatest._

abstract class RBTTreeTestSpec extends FlatSpec
  with Matchers with OptionValues with Inside with Inspectors

class RBTTreeTest extends RBTTreeTestSpec {
  val doc = ""
  val tree: RBT[StringItem] = RBTLeaf() +
    Token(doc, "World", 1) +
    Token(doc, "Best", 2) +
    Token(doc, "Brave", 3) +
    Token(doc, "World!", 4) +
    Token(doc, "Bingo", 5) +
    Token(doc, "Time", 6) +
    Token(doc, "Money", 7) +
    Token(doc, "Floyd", 8) +
    Token(doc, "Pink", 9) +
    Token(doc, "x,", 10) +
    Token(doc, "floyd", 11) +
    Token(doc, "pink", 12) +
    Token(doc, "golden", 13) +
    Token(doc, "boy", 14) +
    Token(doc, "Hell", 15) +
    Token(doc, "Alex", 14)

  val toRemove: RBT[StringItem] = RBTLeaf() +
    Token(doc, "World", 1) +
    Token(doc, "Best", 2) +
    Token(doc, "Brave", 3) +
    Token(doc, "World!", 4) +
    Token(doc, "Bingo", 5)

  val otherDoc = "other"
  val toAdd: RBT[StringItem] = RBTLeaf() +
    Token(otherDoc, "Nothing", 1) +
    Token(otherDoc, "is", 2) +
    Token(otherDoc, "Best", 3) +
    Token(otherDoc, "Than", 4) +
    Token(otherDoc, "Peace", 5)

  val less: RBT[StringItem] = tree -- toRemove
  val more: RBT[StringItem] = tree ++ toAdd

  "tree" should "contains with compare" in {
    assert(tree.contains(t => t.compare(Token(doc, "World", 1))))
    assert(tree.contains(t => t.compare(Token(doc, "Best", 2))))
    assert(tree.contains(t => t.compare(Token(doc, "Brave", 3))))
  }

  "tree" should "contains" in {
    assert(tree.contains(Token(doc, "World", 1)))
    assert(tree.contains(Token(doc, "Best", 2)))
    assert(tree.contains(Token(doc, "Brave", 3)))
  }

  "less" should "not contains with compare" in {
    assert(!less.contains(t => t.compare(Token(doc, "World", 1))))
    assert(!less.contains(t => t.compare(Token(doc, "Best", 2))))
    assert(!less.contains(t => t.compare(Token(doc, "Brave", 3))))
    assert(!less.contains(t => t.compare(Token(doc, "World!", 4))))
    assert(!less.contains(t => t.compare(Token(doc, "Bingo", 4))))
  }

  "less" should "not contains" in {
    assert(!less.contains(Token(doc, "World", 1)))
    assert(!less.contains(Token(doc, "Best", 2)))
    assert(!less.contains(Token(doc, "Brave", 3)))
    assert(!less.contains(Token(doc, "World!", 4)))
    assert(!less.contains(Token(doc, "Bingo", 4)))
  }

  "more" should "contains tree and toAdd" in {
    assert(more.contains(Token(doc, "World", 1)))
    assert(more.contains(Token(doc, "Best", 2)))
    assert(more.contains(Token(doc, "Brave", 3)))
    assert(more.contains(Token(doc, "World!", 4)))
    assert(more.contains(Token(doc, "Bingo", 4)))
    assert(more.contains(Token(otherDoc, "Nothing", 1)))
    assert(more.contains(Token(otherDoc, "is", 2)))
    assert(more.contains(Token(otherDoc, "Best", 3)))
    assert(more.contains(Token(otherDoc, "Than", 4)))
    assert(more.contains(Token(otherDoc, "Peace", 5)))
  }

  "Filtering only item that begin with W" should "contain World and When" in {
    val filter: Token[StringItem] => Boolean = {
      case EmptyToken => false
      case SomeToken(item, _) => item.toUpperCase().startsWith("W")
    }
    val filteredTree = tree.filter(filter)
    assert(filteredTree.contains(t => t.get.compare("World!")))
    assert(filteredTree.contains(t => t.get.compare("World")))
    assert(!filteredTree.contains(t => t.get.compare("When")))
    assert(!filteredTree.contains(t => t.get.compare("Come")))
  }

  "Search" should "find World!" in {
    val result = tree.search((token, local) => local match {
      case EmptyToken => (false, token)
      case SomeToken(item, _) =>
        if (item == token.get) (true, Token(local.get, TInfo.addScore(local.info, 1)))
        else (false, EmptyToken)
    })(Token("World!", List()))
    result match {
      case SomeToken(_, Nil) => throw new IllegalStateException
      case SomeToken(item, info) =>
        assert(info.head.score == 1.0)
        assert(item == "World!")
    }
  }

  "Sorting" should "give a correct order" in {
    val result = tree.sort((l, r) => l.compare(r), t => List(t))
    assert(result.nonEmpty)
    result.iterator.reduce((l, r) => {
      assert(l <= r)
      r
    })
    val tokenInsertOrder = tree.sort((l, r) => (l, r) match {
      case (SomeToken(_, lO), SomeToken(_, rO)) => lO.head.order.compareTo(rO.head.order)
    }, t => t.flatOrders())
    assert(tokenInsertOrder.nonEmpty)
    val last = tokenInsertOrder.iterator.reduce((l, r) => {
      assert(l.info.size == 1)
      assert(r.info.size == 1)
      assert(l.info.head.order <= r.info.head.order)
      r
    })
    val orderedTokens = tree.collectByOrder()
    assert(orderedTokens.nonEmpty)
    println(orderedTokens)
    assert(last match {case SomeToken(i, _) => i == "Hell"})
  }
}
