package grading

object Grade extends edu.unh.cs.mc.grading.GraderApp(
  10 -> new ProblemSuite,
  5 -> new AStarOrderingSuite,

  10 -> new BreadthFirstSuite,
  10 -> new DepthFirstSuite,
  10 -> new AStarSuite,

  5 -> new EarlyFastClosedSetSuite,
  5 -> new LateFastClosedSetSuite,
  5 -> new EarlyClosedSetSuite,

  20 -> new WordLadderSuite,
  20 -> new PathPuzzleSuite
)
