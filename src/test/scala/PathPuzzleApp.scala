import java.net.URL

import puzzles.{ Grid, PathPuzzle4, PathPuzzle8 }
import solvers._

/**
  * Command-line application.
  * {{{command <grid> (4|8) (b|d|a) (n|l|e)}}}
  */
object PathPuzzleApp extends App {
  val grid = Grid(new URL(args(0)))
  val shouldPrint = grid.width <= 100 && grid.height <= 100
  val dir = args(1)
  val search = args(2)
  val closed = args(3)
  val pathPuzzle = (dir, search, closed) match {
    case ("4", "d", "n") => new PathPuzzle4(grid) with DepthFirstSearch
    case ("4", "b", "n") => new PathPuzzle4(grid) with BreadthFirstSearch
    case ("4", "a", "n") => new PathPuzzle4(grid) with AStarSearch
    case ("8", "d", "n") => new PathPuzzle8(grid) with DepthFirstSearch
    case ("8", "b", "n") => new PathPuzzle8(grid) with BreadthFirstSearch
    case ("8", "a", "n") => new PathPuzzle8(grid) with AStarSearch
    case ("4", "d", "s") => new PathPuzzle4(grid) with DepthFirstSearch with EarlyFastClosedSet
    case ("4", "b", "s") => new PathPuzzle4(grid) with BreadthFirstSearch with EarlyFastClosedSet
    case ("4", "a", "s") => new PathPuzzle4(grid) with AStarSearch with EarlyClosedSet
    case ("8", "d", "s") => new PathPuzzle8(grid) with DepthFirstSearch with EarlyFastClosedSet
    case ("8", "b", "s") => new PathPuzzle8(grid) with BreadthFirstSearch with EarlyFastClosedSet
    case ("8", "a", "s") => new PathPuzzle8(grid) with AStarSearch with EarlyClosedSet
    case _               => throw new IllegalArgumentException("cannot parse arguments")
  }
  println(s"${grid.width} x ${grid.height} grid")
  println {
    val s = (grid.startRow, grid.startCol)
    val e = (grid.goalRow, grid.goalCol)
    s"searching $dir-path from $s to $e"
  }
  pathPuzzle.searchPath() match {
    case None => println("no path found")
    case Some(steps) =>
      println(s"path found: ${steps.length} steps")
      if (shouldPrint) {
        for ((row, col) <- steps)
          grid.mark(row, col)
      }
      dir match {
        case "4" => grid.walk4(steps, startToEnd = true)
        case "8" => grid.walk8(steps, startToEnd = true)
      }
  }
  println(f"(${pathPuzzle.searchExpansions}%,d expansions)")
  if (shouldPrint)
    println(grid)
  println(f"time: ${pathPuzzle.searchTime * 1e3}%.1f millis")
}
