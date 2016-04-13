package net.alasc.attributes

import spire.math.SafeLong
import spire.math.prime.{Factors, factor, isPrime}

case class Num(i: SafeLong) extends Attributable {

  type WithAttrs[A] = Num { type Attrs = A }

}

object Num {

  type WithAttrs[A] = Num { type Attrs = A }
  // Properties...

  val factors = Attr[Factors]("factors") // Prime factor decomposition
  val prime = Attr.Bool("prime") // Primality
  val phi = Attr[SafeLong]("phi") // Euler phi function

  // and how to compute them
  implicit val primeCompute: Num.prime.Compute[Num] =
    Num.prime.computeFor[Num] { m => isPrime(m.i) }

  implicit val factorsCompute: Num.factors.Compute[Num] =
    Num.factors.computeFor[Num] { m => factor(m.i) }

  implicit val phiCompute: Num.phi.Compute[Num] =
    Num.phi.computeFor[Num] { m =>
      val f = m.attr(Num.factors)
      f.factors.foldLeft(SafeLong(1)) { case (acc, (prime, power)) => acc * prime.pow(power - 1) * (prime - 1) }
    }

}

object Test {

  val n = Num(123)

  val eleven = Num(11).attr.knowing[Num#WithAttrs](
    Attr.values
      (Num.factors, spire.math.prime.factor(11))
      (Num.prime.True)
  )

  implicitly[Num.prime.TrueFor[eleven.type]]

  implicitly[Num.factors.KnownFor[eleven.type]]

  def f(n: Num)(implicit ev: Num.prime.KnownFor[n.type]): Unit = ()

  implicit def ev = n.attr.remember(Num.prime)

  f(n)

}
