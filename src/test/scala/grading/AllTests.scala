package grading

class AllTests extends org.scalatest.Suites(
  new ProblemSuite,
  new AStarOrderingSuite,

  new BreadthFirstSuite,
  new DepthFirstSuite,
  new AStarSuite,

  new EarlyFastClosedSetSuite,
  new LateFastClosedSetSuite,
  new EarlyClosedSetSuite,

  new WordLadderSuite,
  new PathPuzzleSuite
)
