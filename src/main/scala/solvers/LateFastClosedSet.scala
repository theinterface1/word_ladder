package solvers

import scala.collection.mutable

/**
  * A transformation trait that adds a ''closed set'' to a search algorithm.
  * Fingerprints of nodes are added to the closed set at the time the nodes are expanded (as opposed
  * to when they are created).
  */
trait LateFastClosedSet extends Problem {

  val closedSet:mutable.Set[Node#Fingerprint] = mutable.Set[Node#Fingerprint]()

  /**
    * The given state's fingerprint is added to the closed set and its successors are created and
    * returned.  Only the nodes that are not in the set are returned, in the order in which they
    * were produced.
    */
  override def successors(state: State): Seq[State] = {
    closedSet += state.fingerprint
    for( s <- super.successors(state) if !closedSet.contains(s.fingerprint) ) yield s
  }

  /**
    * Runs the search with a closed set.
    * The search starts with an empty set.
    */
  override def search(start: State): Option[State] = {
    closedSet.clear()

    super.search(start)
  }
}
