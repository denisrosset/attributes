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

    val index = Attribute.nextIndex.getAndIncrement()

    def longKey: Long = (Attributes.this.index.toLong << 32) + (index.toLong & 0xFFFFFFFFL)

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

}
