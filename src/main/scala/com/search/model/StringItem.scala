package com.search.model

import scala.math.Ordered

/**
 * Wrapper to make the String class compatible with the Ordered hierarchy.
 *
 * Scala String is inherited from Java. This induce that String is a Comparable.
 * As Comparable does not accept covariance, String are non variant and this limits the
 * expressiveness of code handling String in Scala. This is particularly obvious when
 * defining case classes that including a type parameterized with Nothing.
 *
 * @param item The String to wrap.
 */
case class StringItem(item: String) extends Ordered[StringItem]{

  /**Return the wrapped String*/
  def get:String = this.item

  def +(other: StringItem):StringItem = new StringItem(this.get + other.get)
  def +(other: String):StringItem = new StringItem(this.get + other)

  /** Result of comparing `this` with operand `that`.
   *
   * Implement this method to determine how instances of A will be sorted.
   *
   * Returns `x` where:
   *
   *   - `x < 0` when `this < that`
   *
   *   - `x == 0` when `this == that`
   *
   *   - `x > 0` when  `this > that`
   *
   */
  override def compare(that: StringItem): Int = this.item.compareTo(that.get)

  /** Returns true if `this` is less than `that`
   */
  override def < (that: StringItem): Boolean = (this compare that) <  0

  /** Returns true if `this` is greater than `that`.
   */
  override def > (that: StringItem): Boolean = (this compare that) >  0

  /** Returns true if `this` is less than or equal to `that`.
   */
  override def <= (that: StringItem): Boolean = (this compare that) <= 0

  /** Returns true if `this` is greater than or equal to `that`.
   */
  override def >= (that: StringItem): Boolean = (this compare that) >= 0

  /** Result of comparing `this` with operand `that`.
   */
  override def compareTo(that: StringItem): Int = this.compare(that)

  override def equals(that: Any): Boolean = that match {
    case StringItem(item) => this.get == item
    case _ => this.get == that
  }
}

object StringItem {
  /**Cast String to StringItem*/
  implicit def string2StringItem(s: String): StringItem = new StringItem(s)
  /**Cast StringItem to String*/
  implicit def StringItem2String(item: StringItem): String = item.get
}
