package com.faacets
package polyta

import java.util.concurrent.locks.ReentrantLock

import scala.language.implicitConversions
import scala.collection.mutable.Map

import spire.util.Opt

final class Evidence[T <: Attributable with Singleton, A <: Attr with Singleton](val ev: AnyRef) extends AnyVal

object Evidence {
  implicit def evidenceFromType[T <: Attributable with Singleton, A <: Attr with Singleton](implicit ev: T#Tags <:< A#Tag): Evidence[T, A] = new Evidence[T, A](ev)
  implicit def evidenceFromCompute[T <: Attributable with Singleton, A <: Attr with Singleton](implicit ev: A#Compute[T]): Evidence[T, A] = new Evidence[T, A](ev)
}

final class AttributableOps[T <: Attributable with Singleton](val t: T) extends AnyVal {

  /** Retrieve from cache or compute from scratch the requested attribute `a`.
    * 
    * In all cases, after calling this method, the cache contains 
    */
  @inline def apply[V](a: Attr.Aux[V])(implicit evidence: Evidence[T, a.type]): V =
    if (evidence.ev.isInstanceOf[<:<[_, _]])
      t._attrApply(a)
    else
      t._attrGetOrElseUpdate(a)(evidence.ev.asInstanceOf[a.Compute[T]])

  /** Finds whether the cache contains the attribute `a`, and returns a reference
    * to this object with updated type tags if the attribute is known.
    */
  @inline def get[V](a: Attr.Aux[V]): Opt[V] = t._attrGet(a)

  @inline def add(a: Attr.Aux[_])(implicit compute: a.Compute[T]): T#WithAttr[a.type] = {
    t._attrGetOrElseUpdate(a)
    t.asInstanceOf[t.WithAttr[a.type]]
  }

  /** Retrieves the value of the boolean attribute `a`, and if `a` is true, returns a reference
    * of `t` tagged with evidence that `a` is true.
    */
  @inline def findIf(a: Attr.Bool with Singleton)(implicit evidence: Evidence[T, a.type]): Opt[T#Is[a.type]] =
    if (apply(a))
      Opt(t.asInstanceOf[t.Is[a.type]])
    else
      Opt.empty[t.Is[a.type]]

  /** Retrieves the value of the boolean attribute `a`, and if `a` is false, returns a reference
    * of this object tagged with evidence that `a` is false.
    */
  @inline def findIfNot(a: Attr.Bool with Singleton)(implicit evidence: Evidence[T, a.type]): Opt[T#IsNot[a.type]] =
    if (!apply(a))
      Opt(t.asInstanceOf[t.IsNot[a.type]])
    else
      Opt.empty[t.IsNot[a.type]]

}

/** Base trait for objects that contain cached attributes. Attributes are properties that are
  * difficult to compute, and/or for which the computation needs external resources or algorithms.
  * 
  * The attribute `a: Attr` can be retrieved in a typesafe manner from an object `t: T` in two cases:
  * 
  * - an implicit instance of `a.Compute[T] ` is available,
  * - there is evidence that `t.Tags <:< a.Tag`.
  * 
  * The attribute cache is never cleaned, so that it can be used in a typesafe manner.
  * 
  */
trait Attributable { self =>

  def attr: AttributableOps[self.type] = new AttributableOps[self.type](self)

  protected val _attrLock = new ReentrantLock()
  protected var _attrDict: Map[String, Any] = Map.empty[String, Any]

  def _attrGetOrElseUpdate[V](a: Attr.Aux[V])(implicit compute: a.Compute[self.type]): V = {
    _attrLock.lock()
    try {
      if (_attrDict.isDefinedAt(a.name))
        _attrDict(a.name).asInstanceOf[V]
      else {
        val result = compute(self)
        _attrDict.put(a.name, result)
        result
      }
    } finally {
      _attrLock.unlock()
    }
  }

  def _attrGet[V](a: Attr.Aux[V]): Opt[V] = {
    _attrLock.lock()
    try {
      if (_attrDict.isDefinedAt(a.name))
        Opt(_attrDict(a.name).asInstanceOf[V])
      else
        Opt.empty[V]
    } finally {
      _attrLock.lock()
    }
  }

  def _attrIsDefined(a: Attr): Boolean = {
    _attrLock.lock()
    try {
      _attrDict.isDefinedAt(a.name)
    } finally {
      _attrLock.unlock()
    }
  }

  def _attrApply[V](a: Attr.Aux[V]): V = {
    _attrLock.lock()
    try {
      _attrDict(a.name).asInstanceOf[V]
    } finally {
      _attrLock.unlock()
    }
  }

  type Tags

  type WithTags[NewTags] = self.type { type Tags = NewTags }

  type WithAttr[A <: Attr with Singleton] = self.type { type Tags = self.Tags with A#Tag }

  type Is[A <: Attr.Bool with Singleton] = self.type { type Tags = self.Tags with A#IsTag }

  type IsNot[A <: Attr.Bool with Singleton] = self.type { type Tags = self.Tags with A#IsNotTag }

}

sealed trait Attr { self =>

  def name: String

  type V

  trait Tag

  trait Compute[-T <: Attributable] {
    def apply(t: T): V
  }

  def apply[T <: Attributable](f: T => V) = new Compute[T] {
    def apply(t: T) = f(t)
  }

}


object Attr {

  type Aux[V0] = Attr { type V = V0 }

  def apply[V0](name0: String): Attr.Aux[V0] = new Attr {
    def name = name0
    type V = V0
  }

  case class Bool(val name: String) extends Attr {
    type V = Boolean
    trait IsTag <: Tag
    trait IsNotTag <: Tag
  }

}
