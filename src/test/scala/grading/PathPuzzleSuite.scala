package grading

import edu.unh.cs.mc.grading.GradingRun
import edu.unh.cs.mc.grading.tagobjects.Fast
import org.scalatest.tagobjects.Slow
import org.scalatest.time.SpanSugar._

class PathPuzzleSuite extends SamplePathPuzzleTests with GradingRun {

  import puzzles.Grid

  import scala.io.Source

  override val longTimeLimit = 10.seconds

  lazy val grid0 = Grid(Source.fromResource("grid0.txt"))

  lazy val grids = {
    List(
      ("grid1", gridByName("grid1.txt"), 39, 30, Fast),
      ("grid2", gridByName("grid2.txt"), 71, 46, Fast),
      ("grid3", gridByName("grid3.txt"), 199, 100, Fast),
      ("grid4", gridByName("grid4.txt"), 474, 334, Fast),
      ("grid5", gridByName("grid5.txt"), 698, 472, Fast),
      ("grid6", gridByName("grid6.txt"), 119, 96, Fast),
      ("grid7", gridByName("grid7.txt"), 207, 172, Fast),
      ("large1", gridByName("large1.txt"), 2003, 1092, Slow)
    )
  }

  test("grid0, 4, breadth-first") {
    assert(new PathBreadth4(grid0).searchPath().isEmpty)
  }

  test("grid0, 4, depth-first") {
    assert(new PathDepth4(grid0).searchPath().isEmpty)
  }

  test("grid0, 4, A star") {
    assert(new PathAStar4(grid0).searchPath().isEmpty)
  }

  for ((name, grid, steps4, steps8, tag) <- grids) {
    test(s"$name, 4, breadth-first", tag) {
      val p = new PathBreadth4(grid)
      val moves = p.searchPath().get
      assert(moves.length === steps4)
      grid.walk4(moves, startToEnd = true)
    }

    test(s"$name, 4, depth-first", tag) {
      val p = new PathDepth4(grid)
      val moves = p.searchPath().get
      assert(moves.length >= steps4)
      grid.walk4(moves, startToEnd = true)
    }

    test(s"$name, 4, A star", tag) {
      val p = new PathAStar4(grid)
      val moves = p.searchPath().get
      assert(moves.length === steps4)
      grid.walk4(moves, startToEnd = true)
    }

    test(s"$name, 8, breadth-first", tag) {
      val p = new PathBreadth8(grid)
      val moves = p.searchPath().get
      assert(moves.length === steps8)
      grid.walk8(moves, startToEnd = true)
    }

    test(s"$name, 8, depth-first", tag) {
      val p = new PathDepth8(grid)
      val moves = p.searchPath().get
      assert(moves.length >= steps8)
      grid.walk8(moves, startToEnd = true)
    }

    test(s"$name, 8, A star", tag) {
      val p = new PathAStar8(grid)
      val moves = p.searchPath().get
      assert(moves.length === steps8)
      grid.walk8(moves, startToEnd = true)
    }
  }
}
