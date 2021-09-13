package solvers

import scala.collection.mutable

/**
  * A transformation trait that adds a ''closed set'' to a search algorithm.
  * Nodes and their fingerprints are added to the closed set at the time the nodes are created
  * (as opposed to when they are expanded).  When nodes are expanded, their children can be used
  * to replace nodes from the closed set if they have a better cost (smaller depth). The initial
  * node is added before the search starts.
  */
trait EarlyClosedSet extends Problem {

  val closedSet:mutable.Map[Node#Fingerprint, Int] = mutable.Map[Node#Fingerprint, Int]()

  /**
    * The successors of the given state are created and added to the closed set.
    * Only the nodes that were not in the set ''and'' the nodes that improve existing nodes in terms
    * of cost are returned, in the order in which they were produced.
    */
  override def successors(state: State): Seq[State] = {
    def hasBeenVisited( s: State ):Boolean = {
      val result = closedSet.contains(s.fingerprint) && (closedSet(s.fingerprint) <= s.depth)
      if(!result) closedSet.put(s.fingerprint,s.depth)
      result
    }

    //println(super.successors(state))
    //println(super.successors(state).map(_.depth))
    //println( closedSet )

    for( x <- super.successors(state) if !hasBeenVisited(x) ) yield x
  }

  /**
    * Runs the search with a closed set.
    * The set is initialized with the starting state before the search starts.
    */
  abstract override def search(start: State): Option[State] = {
    closedSet.clear()
    closedSet.put( start.fingerprint, start.depth )
    super.search(start)
  }
}
