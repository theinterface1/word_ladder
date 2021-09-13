package solvers

import scala.collection.{immutable, mutable}

/**
  * A* search.  This trait can be mixed in a puzzle class to implement an A* search algorithm.
  *
  * The search does not keep track of visited nodes by default.  Traits [[LateFastClosedSet]] or
  * [[EarlyClosedSet]] can be mixed in for this purpose.  [[LateFastClosedSet]] should only be used
  * with consistent heuristics.
  */
trait AStarSearch { self: Problem =>

  /**
    * A* ordering.  Smaller sums depth + heuristic come first.
    */
  val AStarOrdering: Ordering[State] ={
      -f(_) + f(_)
  }

  def f(state: State):Int = {
    state.depth + state.heuristic
  }

  /**
    * A* search.
    */
  def search(start: State): Option[State] = {
    val queue:mutable.PriorityQueue[State] = mutable.PriorityQueue[State]()(AStarOrdering)
    queue.enqueue(start)
    while( queue.nonEmpty ){
      val node = queue.dequeue()
      if( self.isGoal( node ) ){
        return Some( node )
      }
      for( s <- self.successors(node) ) {
        queue.enqueue(s)
      }
    }
    None
  }

}
