package net.alasc.attributes
package typelevel

/** Boolean attribute and a `true` value. */
class TrueValue[A <: Attr[Boolean] with Singleton](val attr: A) extends AnyVal

/** Boolean attribute and a `false` value. */ 
class FalseValue[A <: Attr[Boolean] with Singleton](val attr: A) extends AnyVal
