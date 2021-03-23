package u04lab.code

import Lists.List
import Lists.List._

object SameTeacher {
  def unapply(courses: List[Course]): Option[String] = {
    map(courses)(_.teacher) match {
      case Cons(teacher, t) if allMatch(t)(_ == teacher) => Some(teacher)
      case _ => None
    }
  }
}

object TestSameTeacher {
  import org.junit.jupiter.api.Assertions.{assertTrue, fail}
  import org.junit.jupiter.api.Test

  val cPPS = Course("PPS","Viroli")
  val cOOP = Course("OOP","Viroli")
  val cPCD = Course("PCD","Ricci")

  @Test def testUnapplyMatches(): Unit = {
    List(cPPS, cOOP) match {
      case SameTeacher("Viroli") => assertTrue(true)
      case _ => fail("Should have matched with sameTeacher")
    }
  }

  @Test def testUnapplyDoesNotMatch(): Unit = {
    List(cPPS, cOOP, cPCD) match {
      case SameTeacher(_) => fail("Should not match with sameTeacher")
      case _ => assertTrue(true)
    }
  }

  @Test def testUnapplyOnSingleElementList(): Unit = {
    List(cPPS) match {
      case SameTeacher(_) => assertTrue(true)
      case _ => fail("Should have matched with sameTeacher")
    }
  }
}
