package com.search.model

import org.scalatest._
import flatspec._
import matchers._
import com.search.model.Token.string2Token

abstract class UnitSpec extends AnyFlatSpec
  with should.Matchers with OptionValues with Inside with Inspectors

class TokenTest extends UnitSpec {

  object TestValues {
    var lyrics = "Welcome to the machine"
    var lyricsToken = Token(lyrics, 0)
    var emptyToken:Token[Nothing] = EmptyToken
    var aToken = Token("a", 0)
    var bToken = Token("b", 0)
  }

  "The lyrics token value" should "be == | <= | >= to lyrics" in {
    //assert("Welcome to the machine" == TestValues.lyricsToken)
    assert(TestValues.lyricsToken == Token(TestValues.lyrics, 0))
    assert(TestValues.lyricsToken == TestValues.lyrics)
    assert(!(TestValues.lyrics == TestValues.lyricsToken))
    assert(TestValues.lyrics <= TestValues.lyricsToken)
    assert(TestValues.lyrics <= TestValues.lyricsToken)
    assert(TestValues.lyrics >= TestValues.lyricsToken)
  }

  "Comparing lyricsToken order to EmptyToken" should "fail" in {
    val caughtLess = intercept[Exception]{
      TestValues.lyrics < TestValues.emptyToken
      fail()
    }
    assert(caughtLess.isInstanceOf[NoSuchElementException])

    val caughtLessEqual = intercept[Exception]{
      TestValues.lyrics <= TestValues.emptyToken
      fail()
    }
    assert(caughtLessEqual.isInstanceOf[NoSuchElementException])

    val caughtSup = intercept[Exception]{
      TestValues.lyrics > TestValues.emptyToken
      fail()
    }
    assert(caughtSup.isInstanceOf[NoSuchElementException])

    val caughtSupOrEqual = intercept[Exception]{
      TestValues.lyrics >= TestValues.emptyToken
      fail()
    }
    assert(caughtSupOrEqual.isInstanceOf[NoSuchElementException])
  }

  "EmptyToken" should "be empty and throw Exception when calling get or order" in {
    assert(TestValues.emptyToken.isEmpty)

    val caughtGet = intercept[Exception] {
      TestValues.emptyToken.get
      fail()
    }
    assert(caughtGet.isInstanceOf[NoSuchElementException])

    val caughtOrder = intercept[Exception] {
      TestValues.emptyToken.info.head.order
      fail()
    }
    assert(caughtOrder.isInstanceOf[NoSuchElementException])
  }

  "A" should "not be equal to B" in {
    assert(TestValues.aToken < TestValues.bToken)
    assert(TestValues.aToken <= TestValues.bToken)
    assert(!(TestValues.aToken > TestValues.bToken))
    assert(!(TestValues.aToken >= TestValues.bToken))
    assert(!(TestValues.aToken == TestValues.bToken))
  }

  "An empty string" should "produce an EmptyToken" in {
    val emptyStringToken = Token("")
    assert(emptyStringToken == TestValues.emptyToken)
    assert(emptyStringToken == Token.empty)

    val orderedEmptyStringToken = Token("", 2)
    assert(orderedEmptyStringToken == TestValues.emptyToken)
    assert(orderedEmptyStringToken == Token.empty)
  }

  "Order of non empty String" should "exist if provided" in {
    assert(Token("Foo", 3).info.head.order != List.empty)
    assert(Token("Bar", 2).info.head.order != List.empty)
    assert(Token("Foo", 1).info.head.order != List.empty)
  }

  "String Token" should "be a monad" in {
    def f(x: StringItem, o: List[TInfo]) = Token(x + ", Foo", o)
    def g(x: StringItem, o: List[TInfo]) = Token(x + ", Bar", o)

    // Map and flatMap
    assert(
      Token("Hello").map((i, info) => (i + ", Foo", info))
        ==
        Token("Hello").flatMap((s, i) => Token(s + ", Foo", i))
    )
    // Associativity
    assert(
      TestValues.lyricsToken.flatMap(f).flatMap(g)
        ==
        TestValues.lyricsToken.flatMap((s, o) => f(s, o).flatMap(g))
    )
    // Left Unit
    assert(
      TestValues.lyricsToken.flatMap(f)
        ==
        f(TestValues.lyricsToken.get, TestValues.lyricsToken.info)
    )
    // Right Unit
    assert(
      TestValues.lyricsToken.flatMap(Token(_, _))
        ==
        TestValues.lyricsToken
    )
  }

  "Levensthein distance " should "be be equal to 1 with Fool and Foil" in {
    val left = "Fool"
    val right = "Foil"
    assert(Token.levenshtein(left, right) == 1)
  }

  "Levensthein distance " should "be be equal to 2 with Fool and foil" in {
    val left = "Fool"
    val right = "foil"
    assert(Token.levenshtein(left, right) == 2)
  }

  "Levensthein distance " should "be be equal to 2 with Fool and Fo" in {
    val left = "Fool"
    val right = "Fo"
    assert(Token.levenshtein(left, right) == 2)
  }

  "Levensthein distance " should "be be equal to 3 with Foo and empty string" in {
    val left = "Foo"
    val right = ""
    assert(Token.levenshtein(left, right) == 3)
  }

  "Levensthein distance " should "be be equal to 3 with Foo and Bar" in {
    val left = "Foo"
    val right = "Bar"
    assert(Token.levenshtein(left, right) == 3)
  }
}
