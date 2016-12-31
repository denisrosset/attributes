package net.alasc.attributes

import spire.algebra.EuclideanRing
import spire.math.SafeLong
import spire.math.prime

import org.scalatest.FunSuite

case class Num(i: SafeLong) extends Attributable {

  def isPrime: Boolean = attr(Num.attrs.isPrime)( prime.isPrime(i) )
  def factors: prime.Factors = attr(Num.attrs.factors)( prime.factor(i) )
  def phi: SafeLong = attr(Num.attrs.phi) {
    val f = factors
    f.factors.foldLeft(SafeLong(1)) { case (acc, (prime, power)) => acc * prime.pow(power - 1) * (prime - 1) }
  }

}

object Num {

  // Num attributes

  object attrs extends Attributes("Num") {
    object factors extends Attribute.OfValue[prime.Factors]("factors") // Prime factors decomposition
    object isPrime extends Attribute.Property("isPrime") // Primality
    object phi extends Attribute.OfValue[SafeLong]("phi") // Euler phi function
  }

}

class Test extends FunSuite {

  test("Example for Num") {
    val n = Num(SafeLong(123))
    val eleven = Num(SafeLong(11))
    assert(!n.isPrime)
    assert(eleven.isPrime)
    n.factors
    eleven.factors
    assert(n.attr.isDefined(Num.attrs.factors))
    assert(n.attr.isDefined(Num.attrs.isPrime))
    n.attr(Num.attrs.factors)(sys.error("This path is not taken, as macro inlines a branch"))
  }

  test("Example for SeqEuclideanRing") {
    import spire.implicits._
    val seq = SeqEuclideanRing(Seq(10,25,30,40))
    assert(seq.gcd == 5)
  }

}

case class SeqEuclideanRing[A:EuclideanRing](seq: Seq[A]) extends Attributable {
  def gcd: A = attr(SeqEuclideanRing.attrs.gcd)( seq.foldLeft(EuclideanRing[A].zero)(EuclideanRing[A].gcd) )
}

object SeqEuclideanRing {

  object attrs extends Attributes("SeqEuclideanRing") {
    object gcd extends Attribute("gcd") {
      implicit def forSER[A]: For[SeqEuclideanRing[A], A] = For
    }
  }

}
