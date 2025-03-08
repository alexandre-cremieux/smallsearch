package com.search.engine

import org.scalatest._
import flatspec._
import matchers._

abstract class UnitSpec extends AnyFlatSpec
  with should.Matchers with OptionValues with Inside with Inspectors

class EngineTest extends UnitSpec {
  val items: List[String] = List("Hello", "World", "Best", "Brave", "World")
  val partial: List[String] = List("Hello", "World", "Best", "Brave")
  val toUpperCase: List[String] = List("HELLO", "WORLD", "BEST", "BRAVE")
  val unordered: List[String] = List("Brave", "Best", "World", "Hello")
  val fuzzy: List[String] = List("Hellox", "Wolrd:", "est", "rave", "Bold")
  val garbage: List[String] = List("Hell", "Wolf", "is", "in", "Gold")
  Engine.index("Best Brave World", items.iterator)
  Engine.index("HellWolf", garbage.iterator)

  "Full doc matching" should "return 1" in {
    val result: SearchResult = Engine.exactMatch(items, s => s)
    val docs = result.documents()
    assert(docs.head.getScore == 1)
    System.out.println(result)
  }

  "Full doc fuzzy" should "return 1" in {
    val result: SearchResult = Engine.fuzzy(items, s => s)
    assert(1 == result.documents().head.getScore)
    System.out.println(result)
  }

  "To upper case" should "return 0" in {
    val result: SearchResult = Engine.exactMatch(toUpperCase, s => s)
    assert(result.documents().isEmpty)
    System.out.println(result)
  }

  "Sanitized upper case" should "return 1" in {
    val result: SearchResult = Engine.exactMatch(toUpperCase, s => s.toUpperCase)
    assert(result.documents().head.getScore == 1)
    System.out.println(result)
  }

  "Partial matching" should "return 1" in {
    val result: SearchResult = Engine.exactMatch(partial, s => s)
    assert(result.documents().head.getScore == 1)
    System.out.println(result)
  }

  "Unordered matching" should "return 0.75" in {
    val result: SearchResult = Engine.exactMatch(unordered, s => s)
    assert(result.documents().head.getScore == 0.75)
    System.out.println(result)
  }

  "Fuzzy matching" should "be < 50" in {
    val result: SearchResult = Engine.fuzzy(fuzzy, s => s)
    assert(0.5 > result.documents().head.getScore)
    System.out.println(result)
  }

  "Fuzzy sanitized upper case" should "return 1.0" in {
    val result: SearchResult = Engine.fuzzy(toUpperCase, s => s.toUpperCase)
    System.out.println(result)
    assert(result.documents().head.getScore == 1.0)
  }
}
