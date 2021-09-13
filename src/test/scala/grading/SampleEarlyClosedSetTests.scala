package grading

import org.scalatest.funsuite.AnyFunSuite

class SampleEarlyClosedSetTests extends AnyFunSuite {

  import solvers.EarlyClosedSet

  test("EarlyClosedSet") {
    class P extends AddPuzzle(10, List(1, 2, 3)) {

      def s(n: Int, d: Int) = new AddState(n, d, None)

      def pairs(states: Seq[State]): Seq[(Int, Int)] = for (s <- states) yield s.value -> s.depth

      def search(start: AddState): Option[AddState] = {
        require(start == s(2, 10))
        // closed = {2->10}
        val l1 = pairs(successors(init))
        // closed = {2->1, 1->1, 3->1}
        assert(l1 === Seq(1 -> 1, 2 -> 1, 3 -> 1))
        val l2 = pairs(successors(s(1, 1)))
        // closed = {2->1, 1->1, 3->1, 4->2}
        assert(l2 === Seq(4 -> 2))
        val l3 = pairs(successors(s(2, 5)))
        // closed = {2->1, 1->1, 3->1, 4->2, 5->6}
        assert(l3 === Seq(5 -> 6))
        assert(successors(init).isEmpty)
        assert(successors(s(1, 1)).isEmpty)
        val l4 = pairs(successors(s(2, 3)))
        // closed = {2->1, 1->1, 3->1, 4->2, 5->4}
        assert(l4 === Seq(5 -> 4))
        val l5 = pairs(successors(s(4, 5)))
        // closed = {2->1, 1->1, 3->1, 4->2, 5->4, 6->6, 7->6}
        assert(l5 === Seq(6 -> 6, 7 -> 6))
        assert(successors(s(3, 5)).isEmpty)
        val l6 = pairs(successors(s(3, 3)))
        // closed = {2->1, 1->1, 3->1, 4->2, 5->4, 6->4, 7->6}
        assert(l6 === Seq(6 -> 4))
        assert(successors(s(3, 3)).isEmpty)
        assert(successors(s(4, 5)).isEmpty)
        None
      }
    }

    val p = new P with EarlyClosedSet
    p.search(p.s(2, 10))
    p.search(p.s(2, 10))
  }
}
