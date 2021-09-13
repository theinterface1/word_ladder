package solvers

import scala.annotation.tailrec

/**
  * A search problem as a graph.  The problem is defined as a graph with a starting node and a
  * Boolean condition on a target node.  Nodes keep track of their parent so paths through the
  * graph can be reconstructed.  They also have a `depth` and `heuristic` methods so algorithms
  * like best-first or A* can be used in the search.
  */
abstract class Problem {
  var timeElapsed: Double = -1

  /**
    * A node in the search graph.  Note that the node-returning methods (`successors` and `parent`)
    * return nodes of type `State`, not `Node`, so puzzles can retrieve solutions in terms of their
    * own states.
    */
  trait Node {

    /**
      * The type of the fingerprints.
      *
      * The state of the node itself can be used as its fingerprint if it properly defines equality
      * and hashcode, e.g., `type Fingerprint = State`
      *
      * @see [[fingerprint]]
      */
    type Fingerprint

    /**
      * A fingerprint for the node.  This is used to keep track of visited nodes during a search.
      * It can be exact (i.e., two nodes never have the same fingerprint) or based on hashing, in
      * which case hashing collision can result in an unsuccessful search where a solution exists.
      *
      * The state of the node itself can be used as its fingerprint if it properly defines equality
      * and hashcode, e.g., `def fingerprint = this`
      */
    def fingerprint: Fingerprint

    /**
      * The depth of the node in a path.  This can be used as a (unit) cost in searches.
      * It is implemented here by reconstructing a path back to the starting node.
      * It can be overridden with a more efficient implementation by storing the depth inside a node.
      *
      * @return The depth of this node along a path from the starting point.
      *         The depth of the starting point is 0.
      */
    def depth: Int = {
      @tailrec
      def depthLoop(node: Node, d: Int): Int = node.parent match {
        case None        => d
        case Some(state) => depthLoop(state, d + 1)
      }
      depthLoop(this, 0)
    }

    /**
      * The successors of this node in the search graph.
      * Search algorithms should ''not'' call this method directly but instead rely on
      * [[Problem#successors]] so that transformations to the search can be stacked.
      */
    protected[Problem] def successors: Seq[State]

    /** The previous node along a path from the starting node.  It is `None` for the starting node. */
    def parent: Option[State]

    /**
      * A heuristic.  This is an estimate of the number of steps needed to reach a goal from this node.
      * The default implementation is 0 for a goal and 1 for a non-goal state (which is admissible).
      * It can be overridden in puzzles for which a better heuristic is available.
      */
    def heuristic: Int = if (isGoal) 0 else 1

    /**
      * True iff this node is a goal state.
      * Search algorithms should ''not'' call this method directly but instead rely on
      * [[Problem#isGoal]] so that transformations to the search can be stacked.
      */
    protected[Problem] def isGoal: Boolean
  }

  /** The puzzle state as a node.  It must be a subtype of `Node`. */
  type State <: Node

  /**
    * True iff the given state is a goal state.
    * This method can be overridden for search transformations.
    */
  def isGoal(state: State): Boolean = state.isGoal

  /**
    * The successors to the given state in the search graph.
    * This method can be overridden for search transformations.
    */
  def successors(state: State): Seq[State] = state.successors

  /**
    * The searching method.  This can be implemented directly withing the problem class of supplied
    * by mixing in one of the solver traits.
    *
    * @see [[BreadthFirstSearch]]
    * @see [[DepthFirstSearch]]
    * @see [[AStarSearch]]
    * @param start The starting node for this search.
    * @return A goal state, if one is found.
    */
  def search(start: State): Option[State]

  /** The starting state.  It is used by method `solve` to initiate a search. */
  def init: State

  /**
    * Solves the problem.  This method initiate a search with the given state, using method
    * [[search]].  If no starting state is specified, [[init]] is used.
    *
    * If a target state is found, the corresponding path is reconstructed and returned
    * as a list of states.
    *
    * @return A path to a target state, if one is found.
    */
  def solve(start: State = init): Option[List[State]] = {
    @scala.annotation.tailrec
    def genList(state: Option[State], list: List[State] ): List[State] = {
      state match {
        case None => list
        case Some(s) => genList( s.parent, s ::list )
      }
    }
    val startTime = System.nanoTime()
    val result = search( start )
    val endTime = System.nanoTime()
    timeElapsed = endTime - startTime

    val ret = genList( result, List() )
    if( ret.isEmpty ) None else Some(ret)
  }

  /**
    * Duration of the last search.  It is undefined before the first search.
    * Duration of the last search.  It is undefined before the first search.
    * @return a duration in seconds
    */
  def searchTime: Double = timeElapsed / 1_000_000_000
}
