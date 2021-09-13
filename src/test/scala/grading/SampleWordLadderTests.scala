package grading

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.tagobjects.Slow

class SampleWordLadderTests extends AnyFunSuite {

  import puzzles.WordLadder
  import solvers.{ BreadthFirstSearch, EarlyFastClosedSet }

  val words = List("cat", "man", "mat", "men", "mix", "tan", "ten")

  val graph = Map(
    "mat" -> List("cat", "man"),
    "men" -> List("man", "ten"),
    "man" -> List("mat", "men", "tan"),
    "ten" -> List("men", "tan"),
    "tan" -> List("man", "ten"),
    "cat" -> List("mat")
  )

  test("makeGraph 1") {
    assert(WordLadder.makeGraph(words) === graph + ("mix" -> List()))
  }

  test("makeGraph 2") {
    assert(WordLadder.makeGraph(words, compact = true) === graph)
  }

  test("ladder 1") {
    val wl = new WordLadder("cat", "tan", graph) with BreadthFirstSearch
    assert(wl.ladder.contains(List("cat", "mat", "man", "tan")))
  }

  lazy val (g, gc, g3, g4) = {
    import scala.io.Source
    import scala.util.Using

    val dict = Using.resource(Source.fromResource("COMMON.TXT"))(source => source.getLines().toList)
    val g = WordLadder.makeGraph(dict)
    val gc = WordLadder.makeGraph(dict, compact = true)
    val g3 = WordLadder.makeGraph(dict.filter(_.length == 3), compact = true)
    val g4 = WordLadder.makeGraph(dict.filter(_.length == 4), compact = true)
    (g, gc, g3, g4)
  }

  test("makeGraph 3", Slow) {
    assert(g.size === 74550)
    assert(gc.size === 21228)
    assert(g3.size === 1220)
    assert(g4.size === 3620)
  }

  test("ladder 2") {
    val wl = new WordLadder("pass", "fail", g4) with BreadthFirstSearch
    assert(wl.ladder.contains(List("pass", "bass", "bast", "bait", "bail", "fail")))
  }

  test("ladder 3", Slow) {
    val wl = new WordLadder("black", "white", gc) with BreadthFirstSearch
    assert(wl.ladder.contains(
      List("black", "blank", "blink", "clink", "chink", "chine", "whine", "white"))
    //List("black", "blank", "blink", "clink", "cline", "chine", "whine", "white")
    )
  }

  test("ladder 4") {
    val wl = new WordLadder("black", "white", gc) with BreadthFirstSearch with EarlyFastClosedSet
    assert(wl.ladder.contains(
      List("black", "blank", "blink", "clink", "chink", "chine", "whine", "white"))
    //List("black", "blank", "blink", "clink", "cline", "chine", "whine", "white"))
    )
  }

  test("exceptions 1") {
    assertThrows[IllegalArgumentException] {
      new WordLadder("blue", "red", gc) with BreadthFirstSearch
    }
  }
}

