package net.alasc.attributes

import java.util.concurrent.locks.ReentrantLock

import scala.language.implicitConversions
import scala.collection.mutable.Map

import spire.util.Opt

/** Base trait for objects that contain attributes in a dictionnary. Attributes are properties that are
  * difficult to compute, and/or for which the computation needs external resources or algorithms.
  * 
  * The attribute `a: Attr` can be retrieved in a typesafe manner from an object `t: T` in two cases:
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

  protected val _attrLock = new ReentrantLock()
  protected var _attrDict: Map[String, Any] = Map.empty[String, Any]

  def _attrGetOrElseUpdate[V](a: Attr[V])(implicit compute: a.Compute[self.type]): V = {
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

  def _attrGet[V](a: Attr[V]): Opt[V] = {
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

  def _attrIsDefined(a: Attr[_]): Boolean = {
    _attrLock.lock()
    try {
      _attrDict.isDefinedAt(a.name)
    } finally {
      _attrLock.unlock()
    }
  }

  def _attrApply[V](a: Attr[V]): V = {
    _attrLock.lock()
    try {
      _attrDict(a.name).asInstanceOf[V]
    } finally {
      _attrLock.unlock()
    }
  }

}

object Attributable {

  object knownInstance extends Known[Nothing, Nothing, Nothing] {
    override def toString = "Type-level witness"
  }

  object trueInstance extends True[Nothing, Nothing] {
    override def toString = "Type-level witness"
  }

  object falseInstance extends False[Nothing, Nothing] {
    override def toString = "Type-level witness"
  }

  /** Evidence that `A` is in the dictionary of `T`. */
  trait Known[T <: Attributable with Singleton, A <: Attr[V] with Singleton, V]
  def Known[T <: Attributable with Singleton, A <: Attr[V] with Singleton, V]: Known[T, A, V] = knownInstance.asInstanceOf[Known[T, A, V]]

  /** Evidence that `A` is in the dictionary of `T`, and its value is true. */
  trait True[T <: Attributable with Singleton, A <: Attr[Boolean] with Singleton]
  def True[T <: Attributable with Singleton, A <: Attr[Boolean] with Singleton]: True[T, A] = trueInstance.asInstanceOf[True[T, A]]

  /** Evidence that `A` is in the dictionary of `T`, and its value is false. */
  trait False[T <: Attributable with Singleton, A <: Attr[Boolean] with Singleton]
  def False[T <: Attributable with Singleton, A <: Attr[Boolean] with Singleton]: False[T, A] = falseInstance.asInstanceOf[False[T, A]]

  /** Evidence that the attribute of type `A` can be retrieved from the object of type `T`.
    * 
    * - either `ev` is not null, and can be used to compute the attribute value if needed,
    * - or `ev` is null, which by convention witnesses that the attribute value is known.
    */
  final class Evidence[T <: Attributable with Singleton, A <: Attr[V] with Singleton, V](val ev: A#Compute[T]) extends AnyVal

  object Evidence {
    implicit def known[T <: Attributable with Singleton, A <: Attr[V] with Singleton, V]
      (implicit ev: Known[T, A, V]): Evidence[T, A, V] = new Evidence[T, A, V](null)
    implicit def compute[T <: Attributable with Singleton, A <: Attr[V] with Singleton, V]
      (implicit ev: A#Compute[T]): Evidence[T, A, V] = new Evidence[T, A, V](ev)
  }


  final class Ops[T <: Attributable with Singleton](val t: T) extends AnyVal {

    /** Retrieve from cache or compute from scratch the requested attribute `a`.
      * 
      * In all cases, after calling this method, the dictionnary contains the attribute.
      */
    def apply[V](a: Attr[V] with Singleton)(implicit evidence: Evidence[T, a.type, V]): V =
      if (evidence.ev eq null)
        t._attrApply(a)
      else
        t._attrGetOrElseUpdate(a)(evidence.ev)

    /** Returns the value of the attribute `a` if it is known. */
    def get[V](a: Attr[V]): Opt[V] = t._attrGet(a)

    /** Computes the value of the attribute `a` if not known, and returns evidence that the attribute is known. */
    def known[V](a: Attr[V] with Singleton)(implicit compute: a.Compute[T]): a.KnownFor[T] = {
      t._attrGetOrElseUpdate(a)
      a.KnownFor[T]
    }

    /** Retrieves or computes the value of the boolean attribute `a`; if `a` is true, returns evidence of it. */
    def isTrue(a: Attr.Bool with Singleton)(implicit evidence: Evidence[T, a.type, Boolean]): Opt[a.TrueFor[T]] =
      if (apply(a))
        Opt(a.TrueFor[T])
      else
        Opt.empty[a.TrueFor[T]]

    /** Retrieves or computes the value of the boolean attribute `a`; if `a` is false, returns evidence of it. */
    def isFalse(a: Attr.Bool with Singleton)(implicit evidence: Evidence[T, a.type, Boolean]): Opt[a.FalseFor[T]] =
      if (!apply(a))
        Opt(a.FalseFor[T])
      else
        Opt.empty[a.FalseFor[T]]

  }

}
