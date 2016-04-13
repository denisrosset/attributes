package net.alasc.attributes
package typelevel

object AttrValueList {

  sealed trait Node {

    def apply[V](attr: Attr[V] with Singleton, value: V): Cons[attr.type, Node.this.type, V] =
      ConsValue[attr.type, Node.this.type, V](attr, value, Node.this)

    def apply[A <: Attr[Boolean] with Singleton](trueValue: typelevel.TrueValue[A]): ConsTrue[A, Node.this.type] =
      ConsTrue[A, Node.this.type](trueValue.attr, Node.this)

    def apply[A <: Attr[Boolean] with Singleton](falseValue: typelevel.FalseValue[A]): ConsFalse[A, Node.this.type] =
      ConsFalse[A, Node.this.type](falseValue.attr, Node.this)

    def update(a: Attributable): Unit

  }


  sealed trait Cons[A <: Attr[V] with Singleton, +T <: Node, V] extends Node {

    val attr: A

    def value: V

    def tail: T

    def update(a: Attributable): Unit = {
      a._attrUpdate[V](attr, value)
    }

  }

  final case class ConsValue[A <: Attr[V] with Singleton, +T <: Node, V](val attr: A, val value: V, val tail: T) extends Cons[A, T, V]

  final case class ConsTrue[A <: Attr[Boolean] with Singleton, T <: Node](val attr: A, val tail: T) extends Cons[A, T, Boolean] {

    def value: Boolean = true

  }

  final case class ConsFalse[A <: Attr[Boolean] with Singleton, T <: Node](val attr: A, val tail: T) extends Cons[A, T, Boolean] {

    def value: Boolean = false

  }

  sealed trait Nil extends Node

  case object Nil extends Nil {

    def update(a: Attributable): Unit = { }

  }

  sealed trait Result[-L <: Node, M <: AttrList.Node]

  abstract class Result0 {

    implicit def recurse[A <: Attr[V] with Singleton, T <: Node, M <: AttrList.Node, V](implicit result: Result[T, M]): Result[Cons[A, T, V], AttrList.Cons[A, M]] = null

  }

  object Result extends Result0 {

    /** Retrieves the resulting attribute list for the given attribute value list. Useful for debugging. */
    def forAttrValueList[L <: Node, M <: AttrList.Node](vl: L)(implicit Result: Result[L, M]): Result[L, M] = null

    implicit def nil[L <: AttrValueList.Nil]: Result[L, AttrList.Nil] = null

    implicit def recurseTrue[A <: Attr[Boolean] with Singleton, T <: Node, M <: AttrList.Node](implicit result: Result[T, M]): Result[ConsTrue[A, T], AttrList.ConsTrue[A, M]] = null

    implicit def recurseFalse[A <: Attr[Boolean] with Singleton, T <: Node, M <: AttrList.Node](implicit result: Result[T, M]): Result[ConsFalse[A, T], AttrList.ConsFalse[A, M]] = null

  }

}
