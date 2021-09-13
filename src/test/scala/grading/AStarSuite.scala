package grading

import edu.unh.cs.mc.grading.GradingRun
import org.scalatest.tagobjects.Slow
import org.scalatest.time.SpanSugar._

class AStarSuite extends SampleAStarTests with GradingRun {

  import solvers.{ EarlyClosedSet, LateFastClosedSet }

  override val longTimeLimit = 10.seconds

  test("A* 5") {
    val p = new AddPuzzleAStar(101, List(51, 2, -3))
    assert(p.solution.get.length == 4)
  }

  test("A* 6") {
    val p = new AddPuzzleAStar(101, List(51, -3, 2))
    assert(p.solution.get.length == 4)
  }

  test("A* 7") {
    val p = new AddPuzzleAStar(21, List(2, 6, 8))
    assert(p.solution.isEmpty)
  }

  test("A* 8a") {
    val p = new AddPuzzleAStar(21, List(2, -6, 8)) with LateFastClosedSet
    assert(p.solution.isEmpty)
  }

  test("A* 8b") {
    val p = new AddPuzzleAStar(21, List(2, -6, 8)) with EarlyClosedSet
    assert(p.solution.isEmpty)
  }

  test("A* 10a") {
    val p = new AddPuzzleAStar(101, List(-4, 2, -8, 10, 1))
    assert(p.solution.get.length == 11)
  }

  test("A* 10b") {
    val p = new AddPuzzleAStar(701, List(-4, 2, -8, 10, 1))
    assert(p.solution.get.length == 71)
  }

  test("A* 10c", Slow) {
    val p = new AddPuzzleAStar(1_000_001, List(-4, 2, -8, 10, 1))
    assert(p.solution.get.length == 100_001)
  }

  test("A* 10d", Slow) {
    val p = new AddPuzzleAStar(1_000_001, List(-4, 2, -8, 10, 1)) with LateFastClosedSet
    assert(p.solution.get.length == 100_001)
  }

  test("A* 10e", Slow) {
    val p = new AddPuzzleAStar(1_000_001, List(-4, 2, -8, 10, 1)) with EarlyClosedSet
    assert(p.solution.get.length == 100_001)
  }
}
