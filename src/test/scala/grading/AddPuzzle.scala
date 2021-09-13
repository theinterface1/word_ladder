package grading

import edu.unh.cs.mc.utils.threads.InterruptibleConstructor
import org.scalactic.Requirements._
import solvers._

/**
  * A simple puzzle that can be used to test search algorithms.
  *
  * The puzzle starts at 0 and tries to reach a positive target by adding numbers.
  * The search always stays between 0 and the target.  Given a puzzle with a list of `k` numbers,
  * each node has at most `k` successors.  Furthermore, if all the numbers are positive, the search
  * graph is acyclic.
  *
  * @param target The number to reach (must be positive)
  * @param nums The number that can be added along a path (must be distinct)
  *
  */
abstract class AddPuzzle(target: Int, nums: List[Int]) extends Problem {

  require(target > 0)
  require(nums.nonEmpty)
  require(nums.toSet.size == nums.length)

  private val max = nums.max

  type State = AddState

  /** Puzzle state.  Basically, an integer value, which is also used as the fingerprint. */
  class AddState(val value: Int, override val depth: Int, val parent: Option[AddState])
    extends Node with InterruptibleConstructor {

    private def newState(v: Int) = new AddState(v, depth + 1, Some(this))

    override def toString = s"($value)"

    def successors: List[AddState] =
      for {
        n <- nums
        v = value + n
        if v >= 0 && v <= target
      } yield newState(v)

    /**
      * The heuristic is the smallest number of steps needed to reach the target (in the case where
      * the largest number can be used optimally).
      */
    override def heuristic: Int = {
      val distance = target - value
      if (distance == 0) 0
      else if (distance % max == 0) distance / max
      else distance / max + 1
    }

    type Fingerprint = Int

    def fingerprint: Int = value

    def isGoal: Boolean = value == target

    override def equals(obj: Any): Boolean = obj match {
      case that: AddState => this.value == that.value
      case _              => false
    }

    override def hashCode(): Int = value
  }

  val init: State = new State(value = 0, depth = 0, None)

  def solution: Option[List[Int]] = solve().map { states =>
    for (state <- states.tail) yield state.value - state.parent.get.value
  }

  override def toString: String = s"""AddPuzzle $target with ${nums.mkString(", ")}"""
}

class AddPuzzleBreadth(target: Int, nums: List[Int]) extends AddPuzzle(target, nums)
  with BreadthFirstSearch

class AddPuzzleDepth(target: Int, nums: List[Int]) extends AddPuzzle(target, nums)
  with DepthFirstSearch

class AddPuzzleAStar(target: Int, nums: List[Int]) extends AddPuzzle(target, nums)
  with AStarSearch
