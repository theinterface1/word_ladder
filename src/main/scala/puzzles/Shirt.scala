package puzzles

import solvers.{ DepthFirstSearch, Problem }
import org.scalactic.Requirements._

/**
  * The "shirt" exercise as a depth-first search problem.
  * This results in the same strategy as the plain ''reach'' method from class: try adding first
  * number, then try subtracting if it didn't work.  The main difference is the use of a search
  * stack instead of the execution stack.
  */
class Shirt(target: Int, letters: String) extends Problem with DepthFirstSearch {
  require(letters.nonEmpty)

  type State = ShirtState

  /** Search state, made of a target and remaining numbers. */
  private[Shirt] class ShirtState(
      val target: Int, val nums: List[Int], val parent: Option[ShirtState]
  ) extends Node {

    private def newState(t: Int, l: List[Int]) = new ShirtState(t, l, Some(this))

    /** String representation, including target and numbers to use. */
    override def toString = s"target $target with $nums"

    /** True iff the target is zero. */
    def isGoal: Boolean = nums.isEmpty && target == 0

    /**
      * Children in the search tree.
      * There are at most two children: adding the first number and subtracting the first number.
      * The child that adds the first number is returned first.
      */
    def successors: List[ShirtState] = nums match {
      case Nil           => List.empty
      case first :: more => List(newState(target - first, more), newState(target + first, more))
    }

    type Fingerprint = Long

    /**
      * State fingerprint.  It is made from the target (32 bits) and the length of the list of
      * numbers (32 bits).  It is exact (no collisions).
      */
    def fingerprint: Long = (target.toLong << 32) | nums.length
  }

  /** Starting state: target minus the first letter, to reach with all remaining letters. */
  def init: ShirtState = {
    val first :: others = letters.toList.map(_.toInt): @unchecked
    new ShirtState(target - first, others, None)
  }

  /** Solution to the shirt problem (if any) as a string. */
  def solution: Option[String] = for (path <- solve()) yield {
    val b = new StringBuilder(letters.take(1))
    for (Seq(t1, t2) <- path.iterator.map(_.target).sliding(2)) {
      val c = t1 - t2
      if (c > 0) b += '+' += c.toChar else b += '-' += (-c).toChar
    }
    b.result()
  }
}
