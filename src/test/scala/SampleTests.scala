import grading._

class SampleTests extends org.scalatest.Suites(
  new SampleProblemTests,
  new SampleAStarOrderingTests,

  //new SampleBreadthFirstTests,
  new SampleDepthFirstTests,
  new SampleAStarTests,

  new SampleEarlyFastClosedSetTests,
  new SampleLateFastClosedSetTests,
  new SampleEarlyClosedSetTests,

  new SampleWordLadderTests,
  new SamplePathPuzzleTests
)
