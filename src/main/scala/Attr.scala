package com.faacets
package polyta

import java.util.concurrent.locks.ReentrantLock

import scala.language.existentials
import scala.language.higherKinds
import scala.collection.mutable.Map

import spire.util.Opt

/** Base trait for objects that contain cached attributes. Attributes are properties that are
  * difficult to compute, and/or for which the computation needs external resources or algorithms.
  * 
  * The attribute `a: Attr` can be retrieved in a typesafe manner from an object `t: T` in two cases:
  * 
  * - an implicit instance of `Attr.Compute[a.type, T] ` is available,
  * - there is evidence that `t.Tags <:< a.Tag`.
  * 
  * The attribute cache is never cleaned, so that it can be used in a typesafe manner.
  * 
  */
trait Attributable { self =>

  protected val _attrLock = new ReentrantLock()
  protected var _attrDict: Map[String, Any] = Map.empty[String, Any]

  /** Retrieve from cache or compute from scratch the requested attribute `a`.
    * 
    * In all cases, after calling this method, the cache contains 
    */
  def computeAttr[V](a: Attr.Aux[V])(implicit compute: Attr.Compute[self.type, a.type]): V = {
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

  /** Finds whether the cache contains the attribute `a`, and returns a reference 
    * to this object with updated type tags if the attribute is known.
    */
  def findAttr(a: Attr): Opt[With[a.type]] = {
    _attrLock.lock()
    try {
      if (_attrDict.isDefinedAt(a.name))
        Opt(self.asInstanceOf[With[a.type]])
      else
        Opt.empty[With[a.type]]
    } finally {
      _attrLock.unlock()
    }
  }

  /** Forces the computation of the attribute `a`, and returns a reference to this object
    * with updated type tags. */
  def withAttr(a: Attr)(implicit compute: Attr.Compute[self.type, a.type]): With[a.type] = {
    _attrLock.lock()
    try {
      // force computation
      if (!_attrDict.isDefinedAt(a.name))
        _attrDict.put(a.name, compute(self))
      self.asInstanceOf[With[a.type]]
    } finally {
      _attrLock.unlock()
    }
  }

  /** Retrieves the value of the attribute `a` when there is evidence that the cache
    * contains the attribute. */
  def attr[V](a: Attr.Aux[V])(implicit ev: Tags <:< a.Tag): V = {
    _attrLock.lock()
    try {
      _attrDict(a.name).asInstanceOf[V]
    } finally {
      _attrLock.unlock()
    }
  }

  /** Retrieves the value of the boolean attribute `a`, and if `a` is true, returns a reference
    * of this object tagged with evidence that `a` is true.
    */
  def findIfAttr(a: Attr.Aux[Boolean])(implicit compute: Attr.Compute[self.type, a.type]): Opt[Is[a.type]] =
    if (computeAttr(a))
      Opt(self.asInstanceOf[Is[a.type]])
    else
      Opt.empty[Is[a.type]]

  /** Retrieves the value of the boolean attribute `a`, and if `a` is false, returns a reference
    * of this object tagged with evidence that `a` is false.
    */
  def findIfNotAttr(a: Attr.Aux[Boolean])(implicit compute: Attr.Compute[self.type, a.type]): Opt[IsNot[a.type]] =
    if (!computeAttr(a))
      Opt(self.asInstanceOf[IsNot[a.type]])
    else
      Opt.empty[IsNot[a.type]]

  type Tags

  type Self[NewTags] <: Attributable { type Tags = NewTags }

  type With[A <: Attr with Singleton] = Self[self.Tags with A#Tag]

  type Is[A <: Attr.Aux[Boolean] with Singleton] = Self[self.Tags with A#IsTag]

  type IsNot[A <: Attr.Aux[Boolean] with Singleton] = Self[self.Tags with A#IsNotTag]

}

sealed trait Attr { self =>

  def name: String

  type V

  trait Tag
  trait IsTag <: Tag
  trait IsNotTag <: Tag

  def apply[T <: Attributable](f: T => V): Attr.Compute[T, self.type] = new Attr.Compute[T, self.type] {
    def apply(t: T): V = f(t)
  }

}

object Attr {

  type Aux[V0] = Attr { type V = V0 }

  def apply[V0](name0: String): Attr.Aux[V0] = new Attr {
    def name = name0
    type V = V0
  }

  trait Compute[-T <: Attributable, A <: Attr with Singleton] {

    def apply(t: T): A#V

  }

  def combine[T <: Attributable](x: T, y: T): x.Self[x.Tags with y.Tags] = {
    require(x eq y)
    x.asInstanceOf[x.Self[x.Tags with y.Tags]]
  }    

}
