package u04lab.code

import Lists._
import u04lab.code.Lists.List.{Cons, append, foldLeft, map, nil} // import custom List type (not the one in Scala stdlib)

trait Student {
  def name: String
  def year: Int
  def enrolling(course: Course*): Unit // the student participates to a Course
  def courses: List[String] // names of course the student participates to
  def hasTeacher(teacher: String): Boolean // is the student participating to a course of this teacher?
}

trait Course {
  def name: String
  def teacher: String
}

object Student {
  def apply(name: String, year: Int = 2017): Student = StudentImpl(name, year)

  case class StudentImpl(name: String, year: Int) extends Student {
    private var _courses: List[Course] = List.Nil()

    override def enrolling(coursesToAppend: Course*): Unit = {
      coursesToAppend foreach (course => _courses = append(_courses, Cons(course, nil)))
    }
    override def courses: List[String] = map(_courses)(_.name)
    override def hasTeacher(teacher: String): Boolean = foldLeft(map(_courses)(_.teacher == teacher))(false)(_||_)
  }
}

object Course {
  def apply(name: String, teacher: String): Course = CourseImpl(name, teacher)

  case class CourseImpl(name: String, teacher: String) extends Course
}

object Test {
  import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertTrue}
  import org.junit.jupiter.api.{BeforeEach, Test}

  val cPPS = Course("PPS","Viroli")
  val cPCD = Course("PCD","Ricci")
  val cSDR = Course("SDR","D'Angelo")
  var student: Student = Student("mario", 2015)

  @BeforeEach def beforeEach(): Unit = {
    student = Student("mario", 2015)
  }

  @Test def testEnrolling(): Unit = {
    Array(cPPS, cPCD, cSDR) foreach (student.enrolling(_))
    assertEquals(Cons(cPPS.name, Cons(cPCD.name, Cons(cSDR.name, nil))), student.courses)
  }

  @Test def testVarargEnrolling(): Unit = {
    student.enrolling(cPPS, cPCD, cSDR)
    assertEquals(Cons(cPPS.name, Cons(cPCD.name, Cons(cSDR.name, nil))), student.courses)
  }

  @Test def testHasTeacher(): Unit = {
    student.enrolling(cPPS)
    assertTrue(student.hasTeacher(cPPS.teacher))
    assertFalse(student.hasTeacher(cPCD.teacher))
    assertFalse(student.hasTeacher(cSDR.teacher))
  }
}

/*
object Try extends App {
  val cPPS = Course("PPS","Viroli")
  val cPCD = Course("PCD","Ricci")
  val cSDR = Course("SDR","D'Angelo")
  val s1 = Student("mario",2015)
  val s2 = Student("gino",2016)
  val s3 = Student("rino") //defaults to 2017
  s1.enrolling(cPPS)
  s1.enrolling(cPCD)
  s2.enrolling(cPPS)
  s3.enrolling(cPPS)
  s3.enrolling(cPCD)
  s3.enrolling(cSDR)
  println(s1.courses, s2.courses, s3.courses) // (Cons(PCD,Cons(PPS,Nil())),Cons(PPS,Nil()),Cons(SDR,Cons(PCD,Cons(PPS,Nil()))))
  println(s1.hasTeacher("Ricci")) // true
}
 */

/** Hints:
  * - simply implement Course, e.g. with a case class
  * - implement Student with a StudentImpl keeping a private Set of courses
  * - try to implement in StudentImpl method courses with map
  * - try to implement in StudentImpl method hasTeacher with map and find
  * - check that the two println above work correctly
  * - refactor the code so that method enrolling accepts a variable argument Course*
  */
