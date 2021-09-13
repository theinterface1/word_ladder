package grading

import org.scalatest.funsuite.AnyFunSuite

class SampleEarlyFastClosedSetTests extends AnyFunSuite {

  import solvers.EarlyFastClosedSet

  test("EarlyFastClosedSet") {
    class P extends AddPuzzle(10, List(1, 2, 3)) {

      def s(n: Int) = new AddState(n, 0, None)

      def search(start: AddState): Option[AddState] = {
        require(start == s(2))
        // closed = {2}
        val l1 = successors(init)
        // closed = {2, 1, 3}
        assert(l1 === Seq(s(1), s(3)))
        val l2 = successors(s(1))
        // closed = {2, 1, 3, 4}
        assert(l2 === Seq(s(4)))
        val l3 = successors(s(2))
        // closed = {2, 1, 3, 4, 5}
        assert(l3 === Seq(s(5)))
        assert(successors(init).isEmpty)
        assert(successors(s(1)).isEmpty)
        assert(successors(s(2)).isEmpty)
        val l4 = successors(s(4))
        // closed = {2, 1, 3, 4, 5, 6, 7}
        assert(l4 === Seq(s(6), s(7)))
        val l5 = successors(s(5))
        // closed = {2, 1, 3, 4, 5, 6, 7, 8}
        assert(l5 === Seq(s(8)))
        assert(successors(s(3)).isEmpty)
        assert(successors(s(4)).isEmpty)
        assert(successors(s(5)).isEmpty)
        None
      }
    }

    val p = new P with EarlyFastClosedSet
    p.search(p.s(2))
    p.search(p.s(2))
  }
}
