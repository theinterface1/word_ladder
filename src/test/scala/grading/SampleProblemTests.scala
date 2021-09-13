package grading

import org.scalactic.Tolerance._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.tagobjects.Slow

class SampleProblemTests extends AnyFunSuite {

  class P(slow: Boolean) extends AddPuzzle(10, List(1, 2, 3)) {

    val s1 = init
    val s2 = new AddState(5, 1, Some(s1))
    val s3 = new AddState(9, 2, Some(s2))

    def search(start: AddState): Option[AddState] = {
      if (slow) Thread.sleep(1000)
      if (start == s2) None else Some(start)
    }
  }

  test("solve 1") {
    val p = new P(slow = false)
    assert(p.solve(p.s3).get === List(p.s1, p.s2, p.s3))
  }

  test("solve 2") {
    val p = new P(slow = false)
    assert(p.solve().get === List(p.s1))
  }

  test("solve 3") {
    val p = new P(slow = false)
    assert(p.solve(p.s2).isEmpty)
  }

  test("searchTime", Slow) {
    val p = new P(slow = true)
    p.solve()
    assert(p.searchTime === 1.0 +- 0.1)
  }
}
