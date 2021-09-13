package grading

import org.scalatest.funsuite.AnyFunSuite

class SampleAStarOrderingTests extends AnyFunSuite {

  test("ordering 1") {
    val p = new AddPuzzleAStar(100, List(1))
    val o = p.AStarOrdering
    val s1 = new p.State(50, 5, None)
    val s2 = new p.State(60, 7, None)

    println(o.compare(s1, s2))
    println(o.compare(s2, s1))
    println(o.compare(s1, s1))
    println(o.compare(s2, s2))

    assert(o.compare(s1, s2) < 0)
    assert(o.compare(s2, s1) > 0)
    assert(o.compare(s1, s1) === 0)
    assert(o.compare(s2, s2) === 0)
  }
}
