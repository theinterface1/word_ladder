package solvers

import scala.collection.mutable

/**
  * A transformation trait that adds a ''closed set'' to a search algorithm.
  * Fingerprints of nodes are added to the closed set at the time the nodes are created (as opposed
  * to when they are expanded).  The initial node is added before the search starts.
  */
trait EarlyFastClosedSet extends Problem {

  val closedSet:mutable.Set[Node#Fingerprint] = mutable.Set[Node#Fingerprint]()

  /**
    * The successors of the given state are created and their fingerprints added to the closed set.
    * Only the nodes that were not in the set are returned, in the order in which they were
    * produced.
    */
  override def successors(state: State): Seq[State] = {
    val suc = super.successors( state )
    val result = for( s <- suc if !closedSet.contains(s.fingerprint) ) yield s
    for( s <- suc ) closedSet += s.fingerprint
    result
  }

  /**
    * Runs the search with a closed set.
    * The set is initialized with the starting state before the search starts.
    */
  abstract override def search(start: State): Option[State] = {
    closedSet.clear()
    closedSet += start.fingerprint
    super.search(start)
  }
}
