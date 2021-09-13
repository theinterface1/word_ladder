package puzzles

import solvers._

import scala.collection.View.Empty
import scala.collection.immutable.Range
import scala.collection.mutable

/**
  * A word ladder puzzle.  The objective of the puzzle is to go from one word to another by
  * replacing one letter at a time, only using words from a dictionary.  (In this variant, no
  * letter can be added or removed, so all the words in the ladder have the same length.)
  *
  * For instance: `ape, aye, nye, nae, nan, man` is a ladder from `ape` to `man` (assuming a
  * standard English dictionary).
  *
  * The class `Word` that implements the puzzle state is not public.  It defines a consistent
  * heuristic equal to the number of positions where a word differs from the goal.  For instance, if
  * the target is `man`, then `ape` has heuristic 3 and `mat` has heuristic 1.
  *
  * @param first The word to start from; it must be in the dictionary
  * @param last  The target word; it must be in the dictionary and have the same length as the
  *              first word
  * @param graph The dictionary.  It is given as an undirected graph in which words that differ by
  *              exactly one letter are neighbors.
  * @throws IllegalArgumentException if the start and end words are not in the dictionary,
  *                                  or if they have different lengths
  */
abstract class WordLadder(
    first: String, last: String, graph: Map[String, List[String]]
) extends Problem {

  if( first.length != last.length ) throw new IllegalArgumentException( "First and Last must be same length")
  if( !graph.contains(first) || !graph.contains(last) ) throw new IllegalArgumentException( "Word not in dictionary")

  type State = LadderState

  private[WordLadder] class LadderState(
     val target: String, val word:String, val parent: Option[LadderState]
   ) extends Node {

    private def newState(t: String, w:String) = new LadderState( t, w, Some(this))

    override def toString = s"$word to $target with $graph"

    /** True iff the target is zero. */
    def isGoal: Boolean = target == word

    def successors: List[LadderState] = {
      graph(word).map( newState(target,_) )
    }

    type Fingerprint = String

    def fingerprint: String = word
  }

  def init: State = {
    new LadderState(last,first,None)
  }

  /**
    * The ladder, if any is found.
    * This method calls [[solve]] to initiate a search.
    */
  def ladder: Option[List[String]] = solve(init).map(_.map(_.word))
}

/**
  * Companion object of the [[WordLadder]] class.
  */
object WordLadder {

  def differsByOneLetter( s1:String, s2:String ): Boolean = {
    if( s1 == s2 | s1.length != s2.length ) return false
    var mistakes = 0
    for( i <- 0 until s1.length ){
      if(s1.charAt(i) != s2.charAt(i))
        mistakes += 1
      if( mistakes > 1 ) return false
    }
    true
  }

  /**
    * Builds a graph representation of a dictionary.  Each word is associated with a list of
    * words that differ by exactly one letter.  In order to guarantee the reproducibility of search
    * results, these lists are sorted.
    *
    * For instance, if the dictionary contains the words `(cat, man, mat, men, mix, tan, ten)`, the
    * resulting map is:
    * {{{
    * cat->List(mat)
    * man->List(mat, men, tan)
    * mat->List(cat, man)
    * men->List(man, ten)
    * mix->List()
    * tan->List(man, ten)
    * ten->List(men, tan)
    * }}}
    *
    * Note how word `mix` has no neighbor in the graph.  If the `compact` parameter is set to true,
    * such neighborless nodes are omitted from the graph.
    *
    * @param words   The contents of the dictionary.
    * @param compact True if words without neighbors are omitted from the graph.
    * @return The graph as an immutable map.
    */
  def makeGraph(words: Iterable[String], compact: Boolean = false): Map[String, List[String]] = {
    def getNeighbors( word:String, dictionary:Iterable[String] ): List[String] = {
      dictionary.filter( differsByOneLetter(_,word) ).toList
    }

    val neighborhoods = mutable.Map[String,List[String]]()
    val wordToHood = mutable.Map[String,List[String]]()
    //initialize maps
    for( w <- words ){
      for( i <- 0 until w.length ){
        neighborhoods.put( w.updated(i, '*'), List() )
        wordToHood.put( w, List() )
      }
    }
    //Populate maps
    for( w <- words ){
      for( i <- 0 until w.length ){
        val s = w.updated(i, '*')
        wordToHood( w ) =  s :: wordToHood( w )
        neighborhoods( s ) = w :: neighborhoods( s )
      }
    }

    def hoodOrder( s1: String, s2: String ):Boolean = {
      s1.indexOf('*') < s2.indexOf('*')
    }

    val result = words.map( x => (x,
      wordToHood(x).sortWith(hoodOrder).flatMap(neighborhoods(_)).filterNot(_ == x).sorted
    )).toMap
    if(!compact) result
    else result.filterNot( _._2.isEmpty )
  }
}
