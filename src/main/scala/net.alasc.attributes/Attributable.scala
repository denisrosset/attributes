package net.alasc.attributes

import scala.collection.mutable.LongMap
import scala.language.experimental.macros

import spire.util.Opt

/** Base trait for objects that contain attributes in a dictionnary. Attributes are properties that are
  * difficult to compute, and/or for which the computation needs external resources or algorithms.
  * 
  * The attribute `a: Attribute` can be retrieved in a typesafe manner from an object `t: T` in two cases:
  * 
  * - an implicit instance of `a.Compute[T] ` is available,
  * - there is evidence of type `Known[t.type, a.type]` that the attribute `a` has already been computed for `t`.
  * 
  * Entries in the dictionary are never deleted, so any evidence produced is valid for the lifetime of the object.
  * 
  */
trait Attributable { self =>

  /** Access attribute methods. */
  def attr: Attributable.Ops[self.type] = new Attributable.Ops[self.type](self)

  protected var _attrDict: LongMap[Any] = null

  def _attrUpdate[V](a: Attributes#Attribute)(v: => V)(implicit ev: Value[a.type, self.type, V]): V = synchronized {
    if (_attrDict eq null) _attrDict = LongMap.empty[Any]
    val computed = v
    _attrDict.put(a.longKey, computed)
    computed
  }

  def _attrGet[V](a: Attributes#Attribute)(implicit v: Value[a.type, self.type, V]): Opt[V] = synchronized {
    if ((_attrDict ne null) && _attrDict.isDefinedAt(a.longKey))
      Opt(_attrDict(a.longKey).asInstanceOf[V])
    else
      Opt.empty[V]
  }

  def _attrApply[V](a: Attributes#Attribute)(implicit v: Value[a.type, self.type, V]): V = synchronized {
    _attrDict(a.longKey).asInstanceOf[V]
  }


  def _attrIsDefined(a: Attributes#Attribute): Boolean = synchronized {
    if (_attrDict eq null) false else _attrDict.isDefinedAt(a.longKey)
  }

}

object Attributable {

  import spire.macros.compat.Context


  def macroApply[T <: Attributable with Singleton, A <: Attributes#Attribute with Singleton, V:c.WeakTypeTag]
  (c: Context)(a: c.Expr[A])(v: c.Expr[V])(value: c.Expr[Value[A, T, V]]): c.Expr[V] = {
    import c.universe._
    val lhs = c.prefix.tree
    val tagV = implicitly[c.WeakTypeTag[V]]
    val tree = q"if ($lhs.t._attrIsDefined($a)) $lhs.t._attrApply[$tagV]($a)($value) else $lhs.t._attrUpdate($a)($v)($value)"
    c.Expr[V](tree)
  }

  final class Ops[T <: Attributable with Singleton](val t: T) extends AnyVal {

    /** Returns the value of the attribute `a` if it is known. */
    def get[V](a: Attributes#Attribute)(implicit value: Value[a.type, T, V]): Opt[V] =
      t._attrGet(a: a.type)(value)


    /** Returns the value of the attribute `a` if it is known, otherwise computes it. */
    def apply[A <: Attributes#Attribute with Singleton, V](a: A)(v: V)(implicit value: Value[a.type, T, V]): V = macro macroApply[T, A, V]

    def isDefined[V](a: Attributes#Attribute): Boolean = t._attrIsDefined(a)

  }

}
