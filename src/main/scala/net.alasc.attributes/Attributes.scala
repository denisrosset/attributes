package net.alasc.attributes

import java.util.concurrent.atomic.AtomicInteger

/** Marker trait that computes the result type of Attribute. */
trait Value[A <: Attributes#Attribute with Singleton, -C <: Attributable, V]

class Attributes(val name: String) {

  val index = Attributes.nextIndex.getAndIncrement()

  /** An attribute (abbreviated `Attribute`) is a property of an object that can be
    * computed and cached.
    *
    * The object will cache forever the attributes that have been computed.
    */
  class Attribute(val name: String) {

    override def toString = Attributes.this.name + "." + name

    // operations

    /** Returns the value of the attribute `a` if it is known. */
    def get[T <: Attributable, V](t: T)(implicit value: Value[this.type, T, V]): Option[V] =
      t._attrGet(this: this.type)(value)

    /** Returns the value of the attribute `a` if it is known, otherwise computes it. */
    def apply[T <: Attributable, V](t: T)(v: => V)(implicit ev: Value[this.type, T, V]): V =
      macro Attributes.macroApply[this.type, T, V]

    def isDefined[T <: Attributable](t: T): Boolean = t._attrIsDefined(this)

    // internal bookkeeping

    val index = Attribute.nextIndex.getAndIncrement()

    def longKey: Long = (Attributes.this.index.toLong << 32) + (index.toLong & 0xFFFFFFFFL)

    // polymorphic return values

    type For[C <: Attributable, V] = Value[this.type, C, V]

    def For[C <: Attributable, V]: For[C, V] = null

  }

  object Attribute {

    val nextIndex = new AtomicInteger

    /** An attribute whose value is of constant type V. */
    class OfValue[V](name: String) extends Attribute(name) {
      implicit def forAll[C <: Attributable]: Value[this.type, C, V] = null
    }

    /** An attribute whose value is always Boolean. */
    class Property(name: String) extends Attribute(name)

    object Property {
      implicit def value[A <: Property with Singleton, C <: Attributable]: Value[A, C, Boolean] = null
    }

  }

}

object Attributes {

  val nextIndex = new AtomicInteger

  import Compat._

  def macroApply[A <: Attributes#Attribute with Singleton, T <: Attributable, V:c.WeakTypeTag]
  (c: Context)(t: c.Expr[T])(v: c.Expr[V])(ev: c.Expr[Value[A, T, V]]): c.Expr[V] = {
    import c.universe._
    val tl = freshTermName(c)("t$")
    val al = freshTermName(c)("a$")
    val vl = freshTermName(c)("v$")
    val evl = freshTermName(c)("ev$")
    val lhs = c.prefix.tree
    val tagV = implicitly[c.WeakTypeTag[V]]
    val tree = q"""
{
  val $al = $lhs
  val $tl = $t
  val $evl = $ev
  if ($tl._attrIsDefined($al))
    $tl._attrApply[$tagV]($al)($evl)
  else
    $tl._attrGetOrUpdate[$tagV]($al)($v)($evl)
}
"""
    //    c.info(c.enclosingPosition, showCode(tree), true)
    c.Expr[V](tree)
  }

}
