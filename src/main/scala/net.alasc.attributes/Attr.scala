package net.alasc.attributes

import net.alasc.attributes.typelevel._

/** An attribute (abbreviated `Attr`) is a property of an object that can be
  * computed and cached.
  * 
  * The object will cache forever the attributes that have been computed, but
  * most often cannot compute itself the missing attributes.
  * 
  * Thus, attributes cannot be retrieved in a typesafe manner in general. There
  * are three ways to retrieve an attribute `a: Attr[V]` from an object `t` :
  * 
  * 1. when the attribute is part of the attribute list `Attrs` of the object,
  * 
  * 2. when an implicit `Known[t.type, a.type, V]` is available, evidencing that
  *    `t` contains the value for attribute `a`,
  * 
  * 3. when an implicit instance of `a.Compute[t.type]` is available.
  */
class Attr[V](val name: String) {

  type KnownFor[T <: Attributable with Singleton] = Attributable.Known[T, this.type, V]

  protected[attributes] def KnownFor[T <: Attributable with Singleton]: KnownFor[T] = Attributable.Known.getInstance[T, this.type, V]

  trait Compute[-T <: Attributable] {
    def apply(t: T): V
  }

  def computeFor[T <: Attributable](f: T => V) = new Compute[T] {
    def apply(t: T) = f(t)
  }

}

object Attr {

  def values: AttrValueList.Nil.type = AttrValueList.Nil

  def apply[V](name: String): Attr[V] = new Attr[V](name)

  class Bool(name: String) extends Attr[Boolean](name) {

    def True: TrueValue[Bool.this.type] = new TrueValue(Bool.this)

    def False: FalseValue[Bool.this.type] = new FalseValue(Bool.this)

    type TrueFor[T <: Attributable with Singleton] = Attributable.True[T, this.type]
    protected[attributes] def TrueFor[T <: Attributable with Singleton]: TrueFor[T] = Attributable.True.getInstance[T, this.type]

    type FalseFor[T <: Attributable with Singleton] = Attributable.False[T, this.type]
    protected[attributes] def FalseFor[T <: Attributable with Singleton]: FalseFor[T] = Attributable.False.getInstance[T, this.type]

  }

  object Bool {

    def apply(name: String): Bool = new Bool(name)

  }

}
