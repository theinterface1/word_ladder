package puzzles

import java.net.URL

import org.scalactic.Requirements._

import scala.annotation.tailrec

/**
  * A grid.
  *
  * The grid is mutable in a limited way: Empty spaces can be marked with a dot to display a path.
  * This marking is purely cosmetic and does not change the semantics of a grid in any way.
  *
  * Unless noted otherwise, queries outside the grid throw an `IllegalArgumentException`.
  *
  * Note: The primary constructor is private and the array argument is not copied.
  */
final class Grid private (grid: Array[Array[Byte]]) {

  import Grid._

  /** Height of the grid (number of rows). */
  val height: Int = grid.length

  /** Width of the grid (number of columns). */
  val width: Int = if (grid.isEmpty) 0 else grid(0).length

  /**
    * Grid query.
    * Note that this method returns false but ''does not'' throw an exception for coordinates
    * outside the grid.
    *
    * @return true if the grid is open at the specified location (not outside and not a wal).
    */
  def apply(row: Int, col: Int): Boolean =
    row >= 0 && row < height && col >= 0 && col < width && grid(row)(col) >= 0

  /**
    * Marks en empty location with a dot.  The location is still open.  The walls, starting and
    * ending locations cannot be marked; attempting to mark them has no effect.
    */
  def mark(row: Int, col: Int): Unit = {
    require(row >= 0 && row < height && col >= 0 && col < width)
    if (grid(row)(col) == Empty) grid(row)(col) = Path
  }

  /** Clear all path marks (so a grid can be reused for a new search) */
  def clearMarks(): Unit =
    for (row <- 0 until height; col <- 0 until width)
      if (grid(row)(col) == Path) grid(row)(col) = Empty

  private def check4Path(path: Iterable[(Int, Int)]): Unit = {
    for (Seq((r1, c1), (r2, c2)) <- path.sliding(2))
      if ((r1 - r2).abs + (c1 - c2).abs != 1)
        throw new AssertionError(s"($r1,$c1) -> ($r2,$c2) not a 4-step")
  }

  private def check8Path(path: Iterable[(Int, Int)]): Unit = {
    for (Seq((r1, c1), (r2, c2)) <- path.sliding(2))
      if (((r1 - r2).abs | (c1 - c2).abs) != 1)
        throw new AssertionError(s"($r1,$c1) -> ($r2,$c2) not a 8-step")
  }

  private def walk(path: Iterable[(Int, Int)], startToEnd: Boolean): Unit = {
    if (startToEnd) {
      if (path.isEmpty)
        throw new AssertionError("start-to-end path empty")
      if (path.head != ((startRow, startCol)))
        throw new AssertionError(s"${path.head} not starting point")
      if (path.last != ((goalRow, goalCol)))
        throw new AssertionError(s"${path.last} not end point")
    }
    for ((row, col) <- path)
      if (!apply(row, col))
        throw new AssertionError(s"($row,$col) not walkable")
  }

  /**
    * Checks that a path is valid (not outside or hitting walls) and only uses 4-steps.
    * If `startToEnd` is set, further checks that the path starts and ends at the right points.
    *
    * @throws AssertionError if the path is not valid
    */
  @throws[AssertionError]("if the path is not valid")
  def walk4(path: Iterable[(Int, Int)], startToEnd: Boolean = false): Unit = {
    walk(path, startToEnd)
    check4Path(path)
  }

  /**
    * Checks that a path is valid (not outside or hitting walls) and only uses 8-steps.
    * If `startToEnd` is set, further checks that the path starts and ends at the right points.
    *
    * @throws AssertionError if the path is not valid
    */
  @throws[AssertionError]("if the path is not valid")
  def walk8(path: Iterable[(Int, Int)], startToEnd: Boolean = false): Unit = {
    walk(path, startToEnd)
    check8Path(path)
  }

  /**
    * True iff the row and col correspond to the target.
    * Does not throw exceptions for coordinates outside the grid.
    */
  def isTarget(row: Int, col: Int): Boolean = row == goalRow && col == goalCol

  /** Starting position. */
  val (startRow, startCol) = {
    val starts =
      for (row <- 0 until height; col <- 0 until width; if grid(row)(col) == Start) yield (row, col)
    require(starts.length == 1, "grid has multiple starting points")
    starts.head
  }

  /** Ending position. */
  val (goalRow, goalCol) = {
    val ends =
      for (row <- 0 until height; col <- 0 until width; if grid(row)(col) == End) yield (row, col)
    require(ends.length == 1, "grid has multiple end points")
    ends.head
  }

  /**
    * String representation.
    * The whole grid is surrounded with `+----+` and `|`.  Walls are represented as `#`; starting
    * location is `S`; ending location is 'E'; remaining spaces are blank, unless marked with a dot
    * to display a path.
    */
  override def toString: String = {
    val topBottom = "+" + ("-" * width) + "+"
    val b = new StringBuilder((width + 3) * (height + 2))
    b.append(topBottom).append('\n')
    for (row <- grid)
      b.append("|").appendAll(row.view.map(byte2Char)).append("|\n")
    b.append(topBottom).result()
  }
}

/** Companion object of the [[Grid]] class. */
object Grid {
  import scala.io.Source
  import scala.util.Using

  private final val Empty: Byte = 0
  private final val Path: Byte = 1
  private final val Start: Byte = 2
  private final val End: Byte = 3
  private final val Wall: Byte = -1

  private def byte2Char(byte: Byte): Char = byte match {
    case Empty => ' '
    case Path  => '.'
    case Start => 'S'
    case End   => 'E'
    case Wall  => '#'
  }

  private def char2Byte(char: Char): Byte = char match {
    case '#' => Wall
    case 'S' => Start
    case 'E' => End
    case _   => Empty
  }

  private val topLine = """(.*)(\+-*\+).*""".r

  /**
    * Builds a grid from a textual source.  The format looks like this:
    * {{{
    *
    *             txt
    *      +------------------------+
    *      |      S #               |
    *  txt |   ######    ########   |
    *      |      #        #        |
    *      |###   #    #####   #####|
    *      |  #         ##        ##|
    *      |  ###    ####           | txt
    *      |    ######       ###   E|
    *      +------------------------+
    *                     txt
    * }}}
    * Source and destination must exist and be unique.
    * Characters other than `#`, `S` or `E` are considered empty spaces.
    * The source argument is not closed.
    */
  def apply(source: Source): Grid = {

    val lines = source.getLines()

    @tailrec
    def findSize(): (Int, String) = {
      require(lines.nonEmpty, "top line not found")
      lines.next() match {
        case topLine(p, line) => (p.length, line)
        case _                => findSize()
      }
    }

    val (pre, top) = findSize()
    val width = top.length - 2
    val buffer = Array.newBuilder[Array[Byte]]

    @tailrec
    def readLines(): Array[Array[Byte]] = {
      require(lines.nonEmpty, "bottom line not found")
      val str = lines.next().slice(pre, pre + width + 2)
      if (str == top) buffer.result()
      else {
        val line = str.toSeq
        require(line.head == '|' && line.last == '|', s"bad format line: '$str'")
        buffer += line.slice(1, width + 1).map(char2Byte).toArray
        readLines()
      }
    }

    new Grid(readLines())
  }

  /**
    * Builds a grid from a textual source.
    * @see `apply(Source)` for details
    */
  def apply(url: URL): Grid = Using.resource(Source.fromURL(url))(apply)
}
