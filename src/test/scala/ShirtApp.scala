import puzzles.Shirt
import solvers.EarlyFastClosedSet

/**
  * Command-line application for the ''shirt'' problem.
  * {{{command target letters [closed]}}}
  *
  * If a third argument is present, a closed set is added to the search.
  */
object ShirtApp extends App {

  val target = args(0).toInt
  val letters = args(1).trim
  val shirtSearch =
    if (args.length > 2) new Shirt(target, letters) with EarlyFastClosedSet
    else new Shirt(target, letters)

  println(shirtSearch.solution.getOrElse("no solution"))
  println(f"time: ${shirtSearch.searchTime * 1e3}%.1f millis")
}
