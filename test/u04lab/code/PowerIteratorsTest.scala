package u04lab.code

import Optionals._
import Lists._
import Lists.List._
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class PowerIteratorsTest {

  val factory = new PowerIteratorsFactoryImpl()

  @Test def testIncremental() {
    val pi = factory.incremental(5,_+2); // pi produce 5,7,9,11,13,...
    assertEquals(Option.of(5), pi.next());
    assertEquals(Option.of(7), pi.next());
    assertEquals(Option.of(9), pi.next());
    assertEquals(Option.of(11), pi.next());
    assertEquals(List.Cons(5, List.Cons(7, List.Cons(9, List.Cons(11,List.Nil())))), pi.allSoFar()); // elementi già prodotti
    (0 until 10) foreach (_ => pi.next()) // procedo in avanti per un po'..
    assertEquals(Option.of(33), pi.next()); // sono arrivato a 33
  }

  @Test def testFromList(): Unit = {
    val l = Cons(10, Cons(20, Cons(30, Nil())))

    val iterator = factory.fromList(l)
    forEach(l)(x => assertEquals(Option.of(x), iterator.next()))
    assertEquals(Option.empty, iterator.next())
    assertEquals(l, iterator.allSoFar())

    val inverse = iterator.reversed()
    forEach(reverse(l))(x => assertEquals(Option.of(x), inverse.next()))
    assertEquals(Option.empty, inverse.next())
    assertEquals(reverse(l), inverse.allSoFar())
  }

  @Test def testRandomBool(): Unit = {
    val expectedLen = 10
    val iterator = factory.randomBooleans(expectedLen)
    for (_ <- 1 to expectedLen) iterator.next()
    assertEquals(Option.empty, iterator.next())
    val booleans = iterator.allSoFar()
    assertEquals(expectedLen, length(booleans))

    val inverse = iterator.reversed()
    for (_ <- 1 to expectedLen) inverse.next()
    assertEquals(Option.empty, inverse.next())
    val invertedBooleans = inverse.allSoFar()
    assertEquals(expectedLen, length(invertedBooleans))
    assertEquals(booleans, reverse(invertedBooleans))
  }
}
