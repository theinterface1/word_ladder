package grading

import edu.unh.cs.mc.grading.GradingRun

class AStarOrderingSuite extends SampleAStarOrderingTests with GradingRun {

  test("ordering 2") {
    val p = new AddPuzzleAStar(100, List(1))
    val o = p.AStarOrdering
    val s1 = new p.State(50, 5, None)
    val s2 = new p.State(60, 21, None)
    assert(o.compare(s1, s2) > 0)
    assert(o.compare(s2, s1) < 0)
    assert(o.compare(s1, s1) === 0)
    assert(o.compare(s2, s2) === 0)
  }
}
