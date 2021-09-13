package grading

import org.scalatest.funsuite.AnyFunSuite

class SampleAStarTests extends AnyFunSuite {

  import solvers.{ EarlyClosedSet, LateFastClosedSet }

  test("A* 1") {
    val p = new AddPuzzleAStar(5, List(1, 2))
    assert(p.solution.get.length === 3)
  }

  test("A* 2") {
    val p = new AddPuzzleAStar(5, List(2, 1))
    assert(p.solution.get.length === 3)
  }

  test("A* 3") {
    val p = new AddPuzzleAStar(5, List(3, -1, 1))
    assert(p.solution.get.length === 3)
  }

  test("A* 4") {
    val p = new AddPuzzleAStar(5, List(3, 1, -1))
    assert(p.solution.get.length === 3)
  }

  test("A* 9a") {
    val p = new AddPuzzleAStar(1000, List(100, 1, 2, 3, 4, 5))
    assert(p.solution.get.length == 10)
  }

  test("A* 9b") {
    val p = new AddPuzzleAStar(1000, List(100, 1, 2, 3, 4, 5)) with LateFastClosedSet
    assert(p.solution.get.length == 10)
  }

  test("A* 9c") {
    val p = new AddPuzzleAStar(1000, List(100, 1, 2, 3, 4, 5)) with EarlyClosedSet
    assert(p.solution.get.length == 10)
  }
}
