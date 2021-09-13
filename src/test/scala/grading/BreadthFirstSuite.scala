package grading

import edu.unh.cs.mc.grading.GradingRun
import org.scalatest.tagobjects.Slow

class BreadthFirstSuite extends SampleBreadthFirstTests with GradingRun {

  import solvers.{ EarlyFastClosedSet, LateFastClosedSet }

  test("breadth-first 5") {
    val p = new AddPuzzleBreadth(101, List(51, 2, -3))
    assert(p.solution.contains(List(51, 2, -3, 51)))
  }

  test("breadth-first 6") {
    val p = new AddPuzzleBreadth(101, List(51, -3, 2))
    assert(p.solution.contains(List(51, -3, 51, 2)))
  }

  test("breadth-first 7") {
    val p = new AddPuzzleBreadth(21, List(2, 6, 8))
    assert(p.solution.isEmpty)
  }

  test("breadth-first 8a") {
    val p = new AddPuzzleBreadth(21, List(2, -6, 8)) with LateFastClosedSet
    assert(p.solution.isEmpty)
  }

  test("breadth-first 8b") {
    val p = new AddPuzzleBreadth(21, List(2, -6, 8)) with EarlyFastClosedSet
    assert(p.solution.isEmpty)
  }

  //test("breadth-first 10a", Slow) {
  //  val p = new AddPuzzleBreadth(101, List(-4, 2, -8, 10, 1))
  //  assert(p.solution.get === List.fill(10)(10) :+ 1)
  //}

  test("breadth-first 10b") {
    val p = new AddPuzzleBreadth(101, List(-4, 2, -8, 10, 1)) with LateFastClosedSet
    assert(p.solution.get === List.fill(10)(10) :+ 1)
  }

  test("breadth-first 10c") {
    val p = new AddPuzzleBreadth(101, List(-4, 2, -8, 10, 1)) with EarlyFastClosedSet
    assert(p.solution.get === List.fill(10)(10) :+ 1)
  }

  test("breadth-first 10d", Slow) {
    val p = new AddPuzzleBreadth(701, List(-4, 2, -8, 10, 1)) with LateFastClosedSet
    assert(p.solution.get === List.fill(70)(10) :+ 1)
  }

  test("breadth-first 10e") {
    val p = new AddPuzzleBreadth(701, List(-4, 2, -8, 10, 1)) with EarlyFastClosedSet
    assert(p.solution.get === List.fill(70)(10) :+ 1)
  }

  test("breadth-first 10f") {
    val p = new AddPuzzleBreadth(100_001, List(-4, 2, -8, 10, 1)) with EarlyFastClosedSet
    assert(p.solution.get.length === 10_001)
  }
}
