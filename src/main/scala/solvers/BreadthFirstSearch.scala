package solvers

import scala.collection.immutable.Queue
import scala.collection.mutable

/**
  * Breadth-first search.  This trait can be mixed in a puzzle class to implement a breadth-first
  * search algorithm.
  *
  * The search does not keep track of visited nodes by default.  Trait [[EarlyFastClosedSet]] can
  * be mixed in for this purpose.
  *
  * For testability purposes, successors of a node are handled in the order in which they are
  * produced.  If a node has children `(a,b,c)`, `a` is explored, then `b`, then `c`, then the
  * children of `a`, then the children of `b`, and so on.
  */
trait BreadthFirstSearch { self: Problem =>

  /**
    * Breadth-first search.
    */
  def search(start: State): Option[State] = {
    val queue = Queue(start)
    @scala.annotation.tailrec
    def searchLoop( q:Queue[State] ): Option[State] ={
      q match {
        case _ if q.isEmpty => None
        case _ =>
          val h = q.head
          if( self.isGoal(h) ) Some(h)
          else searchLoop(q.tail.enqueueAll(self.successors(h)))
      }
    }
    searchLoop(queue)
  }

  def badSearch(start: State): Option[State] = {
    val queue: mutable.Queue[State] = mutable.Queue[State]()
    queue.enqueue( start )
    while( queue.nonEmpty ){
      val node = queue.dequeue()
      if( self.isGoal(node) ){
        return Some(node)
      }
      self.successors(node).foreach( queue.enqueue )
    }
    None
  }
}
