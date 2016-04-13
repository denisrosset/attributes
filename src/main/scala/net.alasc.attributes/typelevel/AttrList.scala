package net.alasc.attributes
package typelevel

import scala.annotation.implicitNotFound

object AttrList {

  sealed trait Node

  sealed trait Cons[H <: Attr[_] with Singleton, +T <: Node] extends Node

  sealed trait ConsTrue[H <: Attr[Boolean] with Singleton, +T <: Node] extends Cons[H, T]

  sealed trait ConsFalse[H <: Attr[Boolean] with Singleton, +T <: Node] extends Cons[H, T]

  sealed trait Nil extends Node

  @implicitNotFound("Implicit not found: net.alasc.attributes.Exists[${L}, ${U}]. You requested an attribute of type ${U}, but there is none in the AttrList ${L}.")
  sealed trait Exists[-L <: Node, U <: Attr[_]]

  object Exists {

    def apply[L <: Node, U <: Attr[_] with Singleton](implicit exists: Exists[L, U]): Exists[L, U] = exists

    implicit def exists[H <: Attr[_] with Singleton, T <: Node]: Exists[H Cons T, H] = null

    implicit def recurse[H <: Attr[_] with Singleton, T <: Node, U <: Attr[_] with Singleton](implicit exists: Exists[T, U]): Exists[H Cons T, U] = null

  }

  sealed trait ExistsTrue[-L <: Node, U <: Attr[Boolean] with Singleton]

  object ExistsTrue {

    def apply[L <: Node, U <: Attr[Boolean] with Singleton](implicit existsTrue: ExistsTrue[L, U]): ExistsTrue[L, U] = existsTrue

    implicit def existsTrue[H <: Attr[Boolean] with Singleton, T <: Node]: ExistsTrue[H ConsTrue T, H] = null

    implicit def recurse[H <: Attr[_] with Singleton, T <: Node, U <: Attr[Boolean] with Singleton](implicit existsTrue: ExistsTrue[T, U]): ExistsTrue[H Cons T, U] = null

  }

  sealed trait ExistsFalse[-L <: Node, U <: Attr[Boolean] with Singleton]

  object ExistsFalse {

    def apply[L <: Node, U <: Attr[Boolean] with Singleton](implicit existsFalse: ExistsFalse[L, U]): ExistsFalse[L, U] =
      existsFalse

    implicit def existsFalse[H <: Attr[Boolean] with Singleton, T <: Node]: ExistsFalse[H ConsFalse T, H] = null

    implicit def recurse[H <: Attr[_] with Singleton, T <: Node, U <: Attr[Boolean] with Singleton](implicit existsFalse: ExistsFalse[T, U]): ExistsFalse[H Cons T, U] = null

  }

}
