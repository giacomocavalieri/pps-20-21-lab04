package u04lab.code

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u04lab.code.Lists.List
import u04lab.code.Lists.List.{Cons, nil}

class OptionalEsTest {
  @Test def testListFactory(): Unit = {
    assertEquals(Cons(1, Cons(2, Cons(3, nil))), List(1, 2, 3))
    assertEquals(nil, List())
  }

}
