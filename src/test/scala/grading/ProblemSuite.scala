package grading

import edu.unh.cs.mc.grading.GradingRun
import org.scalatest.time.SpanSugar._

class ProblemSuite extends SampleProblemTests with GradingRun {

  override val longTimeLimit = 2.seconds

  test("solve 4") {
    val p = new P(slow = false)
    val states = List.iterate(p.s1, 1_000_000)(s => new p.State(s.value + 1, s.depth + 1, Some(s)))
    assert(p.solve(states.last).get === states)
  }
}
