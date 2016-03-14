package com.faacets
package polyta

import spire.math.SafeLong
import spire.math.prime.{Factors, factor, isPrime}

case class Num(i: SafeLong) extends Attributable {

  /** Type constructor. */
  type Self[NewTags] = Num { type Tags = NewTags }

}

object Num {

  val factors = Attr[Factors]("factors")
  val prime = Attr[Boolean]("prime")

  implicit val factorsCompute = factors[Num] { m => factor(m.i) }
  implicit val primeCompute = prime[Num] { m => isPrime(m.i) }

}
