package grading

import edu.unh.cs.mc.grading.GradingRun
import org.scalatest.time.SpanSugar._

class WordLadderSuite extends SampleWordLadderTests with GradingRun {

  import puzzles.WordLadder
  import solvers.{ AStarSearch, BreadthFirstSearch, EarlyFastClosedSet }

  override val longTimeLimit = 20.seconds

  test("makeGraph 4") {
    assert(g("above") === List("abode"))
  }

  test("makeGraph 5") {
    assert(g("below").isEmpty)
    assert(!gc.contains("below"))
  }

  test("ladder 5") {
    val wl = new WordLadder("cat", "ten", graph) with BreadthFirstSearch
    assert(wl.ladder.contains(List("cat", "mat", "man", "men", "ten")))
  }

  test("ladder 6") {
    val wl = new WordLadder("cat", "mix", graph + ("mix" -> List())) with BreadthFirstSearch with EarlyFastClosedSet
    assert(wl.ladder.isEmpty)
  }

  test("ladder 7") {
    val wl = new WordLadder("cat", "dog", g) with BreadthFirstSearch
    assert(wl.ladder.contains(List("cat", "cot", "cog", "dog")))
  }

  test("ladder 8") {
    val wl = new WordLadder("ape", "man", g) with AStarSearch
    assert(wl.ladder.get.length === 6)
  }
  test("ladder 9") {
    val wl = new WordLadder("cat", "dog", g3) with BreadthFirstSearch
    assert(wl.ladder.contains(List("cat", "cot", "cog", "dog")))
  }
  test("ladder 10") {
    val wl = new WordLadder("warm", "cold", g) with BreadthFirstSearch
    assert(wl.ladder.contains(List("warm", "ward", "card", "cord", "cold")))
  }
  test("ladder 11") {
    val wl = new WordLadder("warm", "cold", g4) with BreadthFirstSearch
    assert(wl.ladder.contains(List("warm", "ward", "card", "cord", "cold")))
  }
  test("ladder 12") {
    val wl = new WordLadder("above", "below", g) with BreadthFirstSearch with EarlyFastClosedSet
    assert(wl.ladder.isEmpty)
  }

  test("ladder 13") {
    val wl = new WordLadder("orange", "yellow", gc) with BreadthFirstSearch with EarlyFastClosedSet
    assert(wl.ladder.isEmpty)
  }

  test("exceptions 2") {
    assertThrows[IllegalArgumentException] {
      new WordLadder("orange", "yellow", g4) with BreadthFirstSearch
    }
  }

}

