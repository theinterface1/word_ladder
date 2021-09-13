package grading

import org.scalatest.funsuite.AnyFunSuite

class SampleLateFastClosedSetTests extends AnyFunSuite {

  import solvers.LateFastClosedSet

  test("LateFastClosedSet") {
    class P extends AddPuzzle(10, List(1, 2, 3)) {

      def s(n: Int) = new AddState(n, 0, None)

      def search(start: AddState): Option[AddState] = {
        require(start == s(2))
        // closed = {}
        val l1 = successors(init)
        // closed = {0}
        assert(l1 === Seq(s(1), s(2), s(3)))
        val l2 = successors(s(1))
        // closed = {0, 1}
        assert(l2 === Seq(s(2), s(3), s(4)))
        val l3 = successors(s(2))
        // closed = {0, 1, 2}
        assert(l3 === Seq(s(3), s(4), s(5)))
        assert(successors(init) === Seq(s(3)))
        assert(successors(s(1)) === Seq(s(3), s(4)))
        assert(successors(s(2)) === Seq(s(3), s(4), s(5)))
        val l4 = successors(s(4))
        // closed = {0, 1, 2, 4}
        assert(l4 === Seq(s(5), s(6), s(7)))
        val l5 = successors(s(5))
        // closed = {0, 1, 2, 4, 5}
        assert(l5 === Seq(s(6), s(7), s(8)))
        val l6 = successors(s(3))
        // closed = {0, 1, 2, 4, 5, 3}
        assert(l6 === Seq(s(6)))
        assert(successors(s(2)).isEmpty)
        assert(successors(s(4)) === Seq(s(6), s(7)))
        None
      }
    }

    val p = new P with LateFastClosedSet
    p.search(p.s(2))
    p.search(p.s(2))
  }
}
