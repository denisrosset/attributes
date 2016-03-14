package net.alasc.attributes

import spire.math.SafeLong
import spire.math.prime.{Factors, factor, isPrime}

case class Num(i: SafeLong) extends Attributable

object Num {

  val factors = Attr[Factors]("factors")
  val prime = Attr.Bool("prime")

  implicit val primeCompute = prime[Num] { m => isPrime(m.i) }
  implicit val factorsCompute = Num.factors[Num] { m => factor(m.i) }

}

object Test {

  val n = Num(123)

  def f(n: Num)(implicit ev: Num.prime.KnownFor[n.type]): Unit = ()

  implicit def ev = n.attr.known(Num.prime)
  f(n)

}
