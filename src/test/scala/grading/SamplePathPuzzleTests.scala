package grading

import org.scalatest.funsuite.AnyFunSuite
import puzzles.{ Grid, PathPuzzle4, PathPuzzle8 }
import solvers._

import scala.util.Using

class SamplePathPuzzleTests extends AnyFunSuite {

  import scala.io.Source

  class PathBreadth4(grid: Grid) extends PathPuzzle4(grid)
    with BreadthFirstSearch with EarlyFastClosedSet

  class PathBreadth8(grid: Grid) extends PathPuzzle8(grid)
    with BreadthFirstSearch with EarlyFastClosedSet

  class PathDepth4(grid: Grid) extends PathPuzzle4(grid)
    with DepthFirstSearch with EarlyFastClosedSet

  class PathDepth8(grid: Grid) extends PathPuzzle8(grid)
    with DepthFirstSearch with EarlyFastClosedSet

  class PathAStar4(grid: Grid) extends PathPuzzle4(grid)
    with AStarSearch with EarlyClosedSet

  class PathAStar8(grid: Grid) extends PathPuzzle8(grid)
    with AStarSearch with EarlyClosedSet

  def gridByName(name: String) =
    Using.resource(Source.fromResource(name))(Grid(_))

  lazy val grid8 = gridByName("grid8.txt")

  test("no self-loops") {
    val p4 = new PathBreadth4(grid8)
    val p8 = new PathBreadth8(grid8)
    assert(p4.successors(p4.init).length === 2)
    assert(p8.successors(p8.init).length === 2)
  }

  test("grid8, 4, breadth-first") {
    val p = new PathBreadth4(grid8)
    val moves = p.searchPath().get
    assert(moves.length === 46)
    grid8.walk4(moves, startToEnd = true)
  }

  test(s"grid8, 4, depth-first") {
    val p = new PathDepth4(grid8)
    val moves = p.searchPath().get
    assert(moves.length >= 46)
    grid8.walk4(moves, startToEnd = true)
  }

  test(s"grid8, 4, A star") {
    val p = new PathAStar4(grid8)
    val moves = p.searchPath().get
    assert(moves.length === 46)
    grid8.walk4(moves, startToEnd = true)
  }

  test(s"grid8, 8, breadth-first") {
    val p = new PathBreadth8(grid8)
    val moves = p.searchPath().get
    assert(moves.length === 32)
    grid8.walk8(moves, startToEnd = true)
  }

  test(s"grid8, 8, depth-first") {
    val p = new PathDepth8(grid8)
    val moves = p.searchPath().get
    assert(moves.length >= 32)
    grid8.walk8(moves, startToEnd = true)
  }

  test(s"grid8, 8, A star") {
    val p = new PathAStar8(grid8)
    val moves = p.searchPath().get
    assert(moves.length === 32)
    grid8.walk8(moves, startToEnd = true)
  }
}
