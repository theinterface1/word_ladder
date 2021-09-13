package puzzles

import solvers._

/** Common code to 4-neighbor and 8-neighbor puzzles. */
abstract class PathPuzzle(grid: Grid) extends Problem {

  var explanations:Long = 0

  def moves( pos:(Int,Int) ):List[(Int,Int)]

  type State = PathState

  private[PathPuzzle] class PathState(
      val target: (Int,Int), val position: (Int,Int), val parent: Option[PathState]
    ) extends Node {

    private def newState(t:(Int,Int), p:(Int,Int)) = new PathState( t, p, Some(this))

    override def toString = s"$position to $target with $grid"

    def isGoal: Boolean = target == position

    def successors: List[PathState] = {
      val possible = moves(position)
      possible.filter( z => grid( z._1, z._2 ) ).map( newState( target, _ ) )
    }

    type Fingerprint = (Int,Int)

    def fingerprint: (Int,Int) = position

  }

  /**
    * Number of node expansions during the path finding search.
    * It is undefined before the search.
    */
  def searchExpansions: Long = explanations

  /**
    * Path finding search.
    * The first position in the sequence is the grid's starting position; the last position is the
    * end.
    * This method relies on [[solvers.Problem#solve]].
    */
  def searchPath(): Option[Seq[(Int, Int)]] = solve(init).map(_.map(_.position)) // TO IMPLEMENT or leave abstract

  /** A node that corresponds to the starting position in the grid. */
  def init: State = new PathState((grid.goalRow, grid.goalCol), (grid.startRow, grid.startCol), None )
}

/**
  * Path finding in a grid.  Movement is in 4 possible directions; heuristic is Manhattan distance.
  */
abstract class PathPuzzle4(grid: Grid) extends PathPuzzle(grid) {
  def moves( pos:(Int,Int) ):List[(Int,Int)] = {
    explanations += 4
    val (r,c) = pos
    List((r-1,c), (r+1,c), (r,c-1), (r,c+1) )
  }
}

/**
  * Path finding in a grid.  Movement is in 8 possible directions; heuristic is Chebyshev distance.
  */
abstract class PathPuzzle8(grid: Grid) extends PathPuzzle(grid) {
  def moves( pos:(Int,Int) ):List[(Int,Int)] = {
    explanations += 8
    val (r,c) = pos
    List((r-1,c), (r+1,c), (r,c-1), (r,c+1), (r-1,c-1), (r-1,c+1), (r+1,c-1), (r+1,c+1) )
  }
}
