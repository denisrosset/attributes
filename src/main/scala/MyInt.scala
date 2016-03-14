package com.faacets
package polyta

import spire.math.SafeLong
import spire.math.prime.{Factors, factor, isPrime}

case class Num(i: SafeLong) extends Attributable

object Num {

  val factors = Attr[Factors]("factors")
  val prime = Attr.Bool("prime")

  implicit val factorsCompute = factors[Num] { m => factor(m.i) }
  implicit val primeCompute = prime[Num] { m => isPrime(m.i) }

}
