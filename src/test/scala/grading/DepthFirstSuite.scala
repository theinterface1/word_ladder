package grading

import edu.unh.cs.mc.grading.GradingRun

class DepthFirstSuite extends SampleDepthFirstTests with GradingRun {

  import solvers.{ EarlyFastClosedSet, LateFastClosedSet }

  test("depth-first 5") {
    val p = new AddPuzzleDepth(101, List(51, 2, -3))
    assert(p.solution.contains(51 :: List.fill(25)(2)))
  }

  test("depth-first 6a") {
    // goes up 51 -> 48 -> 99,
    // then down 99 -> 96 -> ... -> 57 -> 54,
    // then 51 has already been visited and is skipped, so 56 is next,
    // then ends with 56 -> 53 -> 50 -> 101
    val p = new AddPuzzleDepth(101, List(51, -3, 2)) with LateFastClosedSet
    assert(p.solution.contains(List(51, -3, 51) ::: List.fill(15)(-3) ::: List(2, -3, -3, 51)))
  }

  test("depth-first 6b") {
    // goes up 51 -> 48 -> 99,
    // then down 99 -> 96 -> ... -> 57 -> 54,
    // then 51 has already been visited and is skipped, so 56 is next,
    // but 53 has already been generated as a child of the first 51, so it is not explored,
    // instead, goes into a search from 58 that produces no solution,
    // then backtracks all the way up to 99 and ends with 99 -> 101
    val p = new AddPuzzleDepth(101, List(51, -3, 2)) with EarlyFastClosedSet
    assert(p.solution.contains(List(51, -3, 51, 2)))
  }

  test("depth-first 7") {
    val p = new AddPuzzleDepth(21, List(2, 6, 8))
    assert(p.solution.isEmpty)
  }

  test("depth-first 8a") {
    val p = new AddPuzzleDepth(21, List(2, -6, 8)) with LateFastClosedSet
    assert(p.solution.isEmpty)
  }

  test("depth-first 8b") {
    val p = new AddPuzzleDepth(21, List(2, -6, 8)) with EarlyFastClosedSet
    assert(p.solution.isEmpty)
  }

  test("depth-first 10a") {
    val p = new AddPuzzleDepth(101, List(-4, 2, -8, 10, 1)) with LateFastClosedSet
    assert(p.solution.get === List.fill(50)(2) :+ 1)
  }

  test("depth-first 10b") {
    // search starts 2 -> 4 -> 6 -> 8
    // then 4, 10 and 2 have already been generated, so move to 18,
    // then 20 -> 22 -> 24 -> 26 (all -4 children are skipped),
    // then 28 has already been generated as a child of 18, so move to 36,
    // then 38 -> 40 -> 42 -> 44 -> 54, because 46 is skipped (child of 36),
    // then 56 -> 58 -> 60 -> 62 -> 72, because 64 is skipped (child of 54),
    // then 74 -> 76 -> 78 -> 80 -> 90, because 82 is skipped (child of 72),
    // then 92 -> 94 -> 96 -> 98 -> 99, because 100 is skipped (child of 90),
    // then 101
    val p = new AddPuzzleDepth(101, List(-4, 2, -8, 10, 1)) with EarlyFastClosedSet
    assert(p.solution.get ===
      List.fill(101 / 18)(List(2, 2, 2, 2, 10)).flatten ::: List(2, 2, 2, 2, 1, 2))
  }

  test("depth-first 10c") {
    val p = new AddPuzzleDepth(701, List(-4, 2, -8, 10, 1)) with LateFastClosedSet
    assert(p.solution.get === List.fill(350)(2) :+ 1)
  }

  test("depth-first 10d") {
    val p = new AddPuzzleDepth(701, List(-4, 2, -8, 10, 1)) with EarlyFastClosedSet
    assert(p.solution.get ===
      List.fill(701 / 18)(List(2, 2, 2, 2, 10)).flatten ::: List(2, 2, 2, 2, 1, 2, 2, 2, 2))
  }
}
