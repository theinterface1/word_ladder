import puzzles.WordLadder
import solvers._

import scala.io.Source

/**
  * Command-line application for the word ladder problem.
  *
  * {{{command <dictionary> <first> <last> (b|d|a)}}}
  */
object WordLadderApp extends App {
  val first = args(1)
  val last = args(2)
  val search = args(3)
  val len = first.length
  val graph = {
    import puzzles.WordLadder.makeGraph
    import scala.util.Using
    Using.resource(Source.fromURL(args(0))) { source =>
      //makeGraph(source.getLines().toList, compact = true)
      makeGraph(source.getLines().filter(_.length == len).toList, compact = true)
    }
  }

  val wordLadderPuzzle = search match {
    case "d" => new WordLadder(first, last, graph) with DepthFirstSearch with EarlyFastClosedSet
    case "b" => new WordLadder(first, last, graph) with BreadthFirstSearch with EarlyFastClosedSet
    case "a" => new WordLadder(first, last, graph) with AStarSearch with EarlyClosedSet
    case _   => throw new IllegalArgumentException("cannot parse arguments")
  }

  println(wordLadderPuzzle.ladder.getOrElse("No solution"))
  println(f"time: ${wordLadderPuzzle.searchTime * 1e3}%.1f millis")
}
