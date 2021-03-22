package u04lab.code

import org.junit.jupiter.api.Assertions.assertNotEquals

trait Complex {
  def re: Double
  def im: Double
  def +(c: Complex): Complex // should implement the sum of two complex numbers..
  def *(c: Complex): Complex // should implement the product of two complex numbers
}

object Complex {
  def apply(re:Double, im:Double):Complex = ComplexImpl(re, im) // Fill here

  private case class ComplexImpl(re: Double, im: Double) extends Complex {
    override def +(c: Complex): Complex = Complex(re + c.re, im + c.im)
    override def *(c: Complex): Complex = Complex(re*c.re - im*c.im, re*c.im + im*c.re)
  }
}

object TestComplex {
  import org.junit.jupiter.api.Assertions.assertEquals
  import org.junit.jupiter.api.Test

  @Test def testSum(): Unit = {
    val c1 = Complex(10, 20)
    val c2 = Complex(20, 10)
    assertEquals(Complex(30, 30), c1 + c2)
  }

  @Test def testProduct(): Unit = {
    val c1 = Complex(1, 2)
    val c2 = Complex(3, 1)
    assertEquals(Complex(1, 7), c1 * c2)
  }

  @Test def testToString(): Unit = {
    assertEquals("Complex(1.0,1.0)", Complex(1, 1))
  }

  @Test def testEquality(): Unit = {
    assertEquals(Complex(10, 20), Complex(10, 20))
    assertNotEquals(Complex(10, 20), Complex(11, 22))
  }
}

/*
object TryComplex extends App {
  val a = Array(Complex(10,20), Complex(1,1), Complex(7,0))
  val c = a(0) + a(1) + a(2)
  println(c, c.re, c.im) // (ComplexImpl(18.0,21.0),18.0,21.0)
  val c2 = a(0) * a(1)
  println(c2, c2.re, c2.im) // (ComplexImpl(-10.0,30.0),-10.0,30.0)
}
*/

/** Hints:
  * - implement Complex with a ComplexImpl class, similar to PersonImpl in slides
  * - check that equality and toString do not work
  * - use a case class ComplexImpl instead, creating objects without the 'new' keyword
  * - check equality and toString now
  */
