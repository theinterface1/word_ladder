package grading

import org.scalatest.funsuite.AnyFunSuite

class SampleDepthFirstTests extends AnyFunSuite {

  test("depth-first 1") {
    val p = new AddPuzzleDepth(5, List(1, 2))
    assert(p.solution.contains(List(1, 1, 1, 1, 1)))
  }

  test("depth-first 2") {
    val p = new AddPuzzleDepth(5, List(2, 1))
    assert(p.solution.contains(List(2, 2, 1)))
  }

  test("depth-first 3") {
    val p = new AddPuzzleDepth(5, List(3, -1, 1))
    assert(p.solution.contains(List(3, -1, 3)))
  }

  test("depth-first 4") {
    val p = new AddPuzzleDepth(5, List(3, 1, -1))
    assert(p.solution.contains(List(3, 1, 1)))
  }

  test("depth-first 9") {
    val p = new AddPuzzleDepth(1000, List(100, 1, 2, 3, 4, 5))
    assert(p.solution.get === List.fill(10)(100))
  }
}
