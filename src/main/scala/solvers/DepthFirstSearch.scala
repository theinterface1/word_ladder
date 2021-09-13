package solvers

import java.util

/**
  * Depth-first search.  This trait can be mixed in a puzzle class to implement a depth-first
  * search algorithm.
  *
  * The search does not keep track of visited nodes by default.  Trait [[EarlyFastClosedSet]] can
  * be mixed in for this purpose.
  *
  * For testability purposes, successors of a node are handled in the order in which they are
  * produced.  If a node has children `(a,b,c)`, `a` (and its children) are explored before `b`
  * and `c`.
  */
trait DepthFirstSearch { self: Problem =>

  /**
    * Depth-first search.
    */
  def search(start: State): Option[State] = {
    val stack = new util.Stack[State]()
    stack.push(start)
    while( !stack.empty() ){
      val node = stack.pop()
      if( self.isGoal(node) )
        return Some(node)
      self.successors(node).reverse.foreach( stack.push )
    }
    None
  }
}
