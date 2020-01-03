package com.search.model

import scala.collection.parallel.ParSeq

sealed trait Color
case object Red extends Color
case object Black extends Color

sealed trait RBT[A <: Ordered[A]] {

  def isEmpty: Boolean

  def color: Color // False for red, true for black

  def left: RBT[A]

  def right: RBT[A]

  def +(that: Token[A]): RBT[A] = that match {
    case EmptyToken => this
    case that: SomeToken[A] => RBT.add(that, this)
  }

  def -(that: Token[A]): RBT[A] = that match {
    case EmptyToken =>  this
    case that: SomeToken[A] => RBT.remove(that, this)
  }

  def ++(that: RBT[A]): RBT[A] = that match {
    case RBTLeaf() => this
    case _ => RBT.filterAcc(_ => true, that, this)
  }

  def --(that: RBT[A]): RBT[A] = that match {
    case RBTLeaf() => this
    case _ => RBT.filterAcc(t => !that.contains(t), this, RBTLeaf())
  }

  def contains(that: Token[A]): Boolean = this.contains(t => t.compare(that))

  def contains(order: Token[A] => Int): Boolean = RBT.contains(order, this)

  def filter(f: Token[A] => Boolean): RBT[A] = RBT.filterAcc(f, this, RBTLeaf())

  def search(scorer: (Token[A], Token[A]) => (Boolean, Token[A]))(init: Token[A]):
    Token[A] = RBT.search(scorer, this)(init)

  def searchToList
  (f:(Token[A], Token[A]) => (Boolean, Token[A]))(tokens: List[Token[A]]):
  List[Token[A]] = {
    if (this.isEmpty) List.empty
    else {
      if (tokens.size <= 2) {
        val search: Token[A] => Token[A] = this.search(f)
        tokens
          .map(search)
          .filter(t => !t.isEmpty)
      } else {
        val lists = tokens.splitAt(tokens.size / 2)
        ParSeq(
          this.searchToList(f)(lists._1),
          this.searchToList(f)(lists._2)
        ).reduce(_ ++ _).distinct
      }
    }
  }

  def sort(f: (Token[A], Token[A]) => Int, m: Token[A] => List[Token[A]]):
    List[Token[A]] = Token.sort(f, RBT.collect(this, m).toArray).toList

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
      case (EmptyToken, EmptyToken) => 0
      case (SomeToken(_, _), EmptyToken) => -1
      case (EmptyToken, SomeToken(_, _)) => 1
      case (SomeToken(_, Nil), SomeToken(_, Nil)) => 0
      case (SomeToken(_, _), SomeToken(_, Nil)) => 1
      case (SomeToken(_, Nil), SomeToken(_, _)) => -1
      case (SomeToken(_, x :: _), SomeToken(_, y :: _)) => x.order.compareTo(y.order)
    }
    this.sort(comparator, token => token.flatOrders())
  }

  override def toString: String = RBT.toString(0, this)
}

case class RBTLeaf[A <: Ordered[A]]() extends RBT[A] {
  override def isEmpty: Boolean = true
  override def color: Color = Black
  override def left: RBT[A] = RBTLeaf()
  override def right: RBT[A] = RBTLeaf()
}

case class RBTBranch[A <: Ordered[A]]
(color: Color, token: Token[A], left: RBT[A], right: RBT[A]) extends RBT[A] {
  override def isEmpty: Boolean = false
}

object RBT {

  def apply[A <: Ordered[A]](c: Color, tree: RBT[A]): RBT[A] =
    tree match {
      case RBTBranch(_, t, l, r) => RBTBranch(c, t, l, r)
      case _ => tree
    }

  def apply[A <: Ordered[A]](token: Token[A]): RBT[A] = token match {
    case EmptyToken => RBTLeaf()
    case token: SomeToken[A] => RBT.add(token, RBTLeaf())
  }

  def add[A <: Ordered[A]](token: SomeToken[A], tree: RBT[A]): RBT[A] = {
    def addRec(inTree: RBT[A]): RBT[A] = {
      inTree match {
        case RBTLeaf() => new RBTBranch[A](Red, token, RBTLeaf(), RBTLeaf())
        case RBTBranch(c, t, l, r) =>
          if (t > token) {
            RBT.cleanTreeLeft(RBTBranch(c, t, addRec(l), r))
          } else if (t < token) {
            RBT.cleanTreeRight(RBTBranch(c, t, l, addRec(r)))
          } else {
            RBTBranch(c, Token.merge(t, token), l, r)
          }
      }
    }
    RBT(Black, addRec(tree))
  }

  def remove[A <: Ordered[A]](token: SomeToken[A], tree: RBT[A]): RBT[A] = {
    def removeRec(inTree: RBT[A]): RBT[A] = inTree match {
      case RBTLeaf() => RBTLeaf()
      case RBTBranch(c, t, l, r) =>
        if (t > token) {
          RBT.cleanTreeLeft(RBTBranch(c, t, removeRec(l), r))
        } else if (t < token) {
          RBT.cleanTreeRight(RBTBranch(c, t, l, removeRec(r)))
        } else {
          l ++ r
        }
    }
    RBT(Black, removeRec(tree))
  }

  @scala.annotation.tailrec
  def contains[A <: Ordered[A]](f: Token[A] => Int, tree: RBT[A]): Boolean = tree match {
    case RBTLeaf() => false
    case RBTBranch(_, t, l, r) =>
      val order = f(t)
      if (order == 0) {
        true
      } else if (order > 0) {
        RBT.contains(f, l)
      } else {
        RBT.contains(f, r)
      }
  }

  def search[A <: Ordered[A]]
  (scorer: (Token[A], Token[A]) => (Boolean, Token[A]), tree: RBT[A])(init: Token[A]):
  Token[A] = tree match {
    case RBTLeaf() => EmptyToken
    case RBTBranch(_, token, l, r) =>
      val (isMatch, output) = scorer(init, token)
      if (isMatch) {
        output
      } else {
        val subResult =
          if (!l.isEmpty && token > init) {
            RBT.search(scorer, l)(init)
          } else if (!r.isEmpty && token < init) {
            RBT.search(scorer, r)(init)
          } else {
            EmptyToken
          }

        (output, subResult) match {
          case (EmptyToken, EmptyToken) => EmptyToken
          case (EmptyToken, _) => subResult
          case (_, EmptyToken) => output
          case _ =>
            if (TInfo.compareScore(subResult.info, output.info) > 0) {
              subResult
            } else {
              output
            }
        }
      }
  }

  def collect[A <: Ordered[A]]
  (tree: RBT[A], f: Token[A] => List[Token[A]]): List[Token[A]] = tree match {
    case RBTLeaf() => List()
    case RBTBranch(_, t, l, r) => ParSeq(
      collect(l, f),
      f(t),
      collect(r, f)
    ).flatten.toList
  }

  private def cleanTreeLeft[A <: Ordered[A]](tree: RBTBranch[A]): RBT[A] =
    tree match {
      case RBTBranch(Black, t, RBTBranch(Red, _, RBTBranch(Red, _, _, _), _),
        _) => rotateRight(
            RBTBranch(Red, t, RBT(Black, tree.left), RBT(Black, tree.right)))
      case RBTBranch(
        Black, t, RBTBranch(Red, _, _, RBTBranch(Red, _, _, _)),
        _) => rotateLeft(
            RBTBranch(Black, t, rotateLeft(RBT(Black, tree.left)), RBT(Red, tree.right)))
      case _ => tree
    }

  private def cleanTreeRight[A <: Ordered[A]](tree: RBTBranch[A]): RBT[A] =
    tree match {
      case RBTBranch(
        Black, t, _, RBTBranch(Red, _, RBTBranch(Red, _, _, _), _)
      ) => rotateLeft(
            RBTBranch(Black, t, RBT(Red, tree.left), rotateRight(RBT(Black, tree.right))))
      case RBTBranch(
        Black, t, _, RBTBranch(Red, _, _, RBTBranch(Red, _, _, _))
      ) => rotateRight(
            RBTBranch(Red, t, RBT(Black, tree.left), RBT(Black, tree.right)))
      case _ => tree
    }

  private def rotateLeft[A <: Ordered[A]](tree: RBT[A]): RBT[A] = tree match {
    case RBTLeaf() => tree
    case RBTBranch(_, _, _, RBTLeaf()) => tree
    case RBTBranch(c, t, l, RBTBranch(rc, rt, rl, rr)) =>
      RBTBranch(rc, rt, RBTBranch(c, t, l, rl), rr)
  }

  private def rotateRight[A <: Ordered[A]](tree: RBT[A]): RBT[A] = tree match {
    case RBTLeaf() => tree
    case RBTBranch(_, _, RBTLeaf(), _) => tree
    case RBTBranch(c, t, RBTBranch(lc, lt, ll, lr), r) =>
      RBTBranch(lc, lt, ll, RBTBranch(c, t, lr, r))
  }

  private def filterAcc[A <: Ordered[A]]
  (f: Token[A] => Boolean, tree: RBT[A], acc: RBT[A]): RBT[A] = tree match {
    case RBTLeaf() => acc
    case RBTBranch(_, t, l, r) =>
      if (f(t)) filterAcc(f, l, filterAcc(f, r, acc + t))
      else filterAcc(f, l, filterAcc(f, r, acc))
  }

  private def toString[A <: Ordered[A]](tab: Int, tree: RBT[A]): String = tree match {
    case RBTLeaf() => "\t" * tab + "Leaf()"
    case RBTBranch(c, t, l, r) =>
      "\t" * tab + c + " " + t + "\n" + toString(tab + 1, l) + "\n" + toString(tab + 1, r)
  }
}
