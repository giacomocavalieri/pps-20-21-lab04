package u04lab.code

import Streams.Stream
import Streams.Stream.{head, tail}
import Lists.List
import Lists.List.{nil, reverse}
import Optionals.Option
import Optionals.Option._

trait PowerIterator[A] {
  def next(): Option[A]
  def allSoFar(): List[A]
  def reversed(): PowerIterator[A]
}

object PowerIterator {
  def apply[A](stream: Stream[A]): PowerIterator[A] = new PowerIteratorImpl(stream)

  class PowerIteratorImpl[A](private var stream: Stream[A]) extends PowerIterator[A] {
    private var generated: List[A] = nil

    override def next(): Option[A] = {
      val nextElement = head(stream)
      stream = tail(stream)
      addToGenerated(nextElement)
      nextElement
    }

    private def addToGenerated(elem: Option[A]): Unit = elem match {
      case Some(a) => generated = List.Cons(a, generated)
      case _ => ()
    }

    override def allSoFar(): List[A] = reverse(generated)
    override def reversed(): PowerIterator[A] = new PowerIteratorsFactoryImpl().fromList(generated)
  }
}

trait PowerIteratorsFactory {
  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  def fromList[A](list: List[A]): PowerIterator[A]
  def randomBooleans(size: Int): PowerIterator[Boolean]
}

class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {
  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] = PowerIterator(Stream.iterate(start)(successive))
  override def fromList[A](list: List[A]): PowerIterator[A] = PowerIterator(Stream.fromList(list))
  override def randomBooleans(size: Int): PowerIterator[Boolean] = PowerIterator(Stream.take(Stream.generate(Math.random() < 0.5))(size))
}
