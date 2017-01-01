package net.alasc.attributes

import org.scalatest.FunSuite

case class Num(i: BigInt) extends Attributable {

  def isPrime: Boolean = Num.attributes.isPrime(this)( i.isProbablePrime(100) )

}

object Num {

  // Num attributes

  object attributes extends Attributes("Num") {
    object isPrime extends Attribute.Property("isPrime") // Primality
  }

}

class Test extends FunSuite {

  test("Example for Num") {
    val n = Num(BigInt(123))
    val eleven = Num(BigInt(11))
    assert(!n.isPrime)
    assert(eleven.isPrime)
    assert(Num.attributes.isPrime.isDefined(n))
    Num.attributes.isPrime(n)(sys.error("This path is not taken, as macro inlines a branch"))
  }

  test("Example for SeqGCD") {
    val seq1 = SeqGCD(Seq(10,25,30,40))
    val seq2 = SeqGCD(Seq(BigInt(10),BigInt(25),BigInt(30),BigInt(40)))
    assert(seq1.gcd == 5)
    assert(seq2.gcd == BigInt(5))
  }

}

/** Barebones type class for a subset of Euclidean ring operations. */
trait GCD[A] {
  def zero: A
  def one: A
  def gcd(x: A, y: A): A
}

object GCD {

  def apply[A](implicit ev: GCD[A]): GCD[A] = ev

  implicit val bigInt: GCD[BigInt] = new GCD[BigInt] {
    val zero: BigInt = BigInt(0)
    val one: BigInt = BigInt(1)
    def gcd(x: BigInt, y: BigInt): BigInt = x.gcd(y)
  }

  implicit val int: GCD[Int] = new GCD[Int] {
    def zero: Int = 0
    def one: Int = 1
    def gcd(x: Int, y: Int): Int = BigInt(x).gcd(BigInt(y)).toInt // easy hack
  }

}

case class SeqGCD[A:GCD](seq: Seq[A]) extends Attributable {
  def gcd: A = SeqGCD.attrs.gcd(this)( seq.foldLeft(GCD[A].zero)(GCD[A].gcd) )
}

object SeqGCD {

  object attrs extends Attributes("SeqGCD") {
    object gcd extends Attribute("gcd") {
      implicit def forSER[A]: For[SeqGCD[A], A] = For
    }
  }

}
