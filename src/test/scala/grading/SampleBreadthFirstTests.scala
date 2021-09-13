package grading

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.tagobjects.Slow

class SampleBreadthFirstTests extends AnyFunSuite {

  import solvers.{ EarlyFastClosedSet, LateFastClosedSet }

  test("breadth-first 1") {
    val p = new AddPuzzleBreadth(5, List(1, 2))
    assert(p.solution.contains(List(1, 2, 2)))
  }

  test("breadth-first 2") {
    val p = new AddPuzzleBreadth(5, List(2, 1))
    assert(p.solution.contains(List(2, 2, 1)))
  }

  test("breadth-first 3") {
    val p = new AddPuzzleBreadth(5, List(3, -1, 1))
    assert(p.solution.contains(List(3, -1, 3)))
  }

  test("breadth-first 4") {
    val p = new AddPuzzleBreadth(5, List(3, 1, -1))
    assert(p.solution.contains(List(3, 1, 1)))
  }

  test("breadth-first 9a", Slow) {
    val p = new AddPuzzleBreadth(1000, List(100, 1, 2, 3, 4, 5))
    assert(p.solution.get === List.fill(10)(100))
  }

  test("breadth-first 9b") {
    val p = new AddPuzzleBreadth(1000, List(100, 1, 2, 3, 4, 5)) with LateFastClosedSet
    assert(p.solution.get === List.fill(10)(100))
  }

  test("breadth-first 9c") {
    val p = new AddPuzzleBreadth(1000, List(100, 1, 2, 3, 4, 5)) with EarlyFastClosedSet
    assert(p.solution.get === List.fill(10)(100))
  }
}
