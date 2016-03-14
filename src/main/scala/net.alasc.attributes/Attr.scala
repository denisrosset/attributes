package net.alasc.attributes

class Attr[V](val name: String) {

  type KnownFor[T <: Attributable with Singleton] = Attributable.Known[T, this.type, V]
  def KnownFor[T <: Attributable with Singleton]: KnownFor[T] = Attributable.Known[T, this.type, V]

  trait Compute[-T <: Attributable] {
    def apply(t: T): V
  }

  def apply[T <: Attributable](f: T => V) = new Compute[T] {
    def apply(t: T) = f(t)
  }

}

object Attr {

  def apply[V](name: String): Attr[V] = new Attr[V](name)

  class Bool(name: String) extends Attr[Boolean](name) {

    type TrueFor[T <: Attributable with Singleton] = Attributable.True[T, this.type]
    def TrueFor[T <: Attributable with Singleton]: TrueFor[T] = Attributable.True[T, this.type]

    type FalseFor[T <: Attributable with Singleton] = Attributable.False[T, this.type]
    def FalseFor[T <: Attributable with Singleton]: FalseFor[T] = Attributable.False[T, this.type]

  }

  object Bool {

    def apply(name: String): Bool = new Bool(name)

  }

}
