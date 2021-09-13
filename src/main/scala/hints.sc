val l = List("foo", "foobar", "X", "Y", "Z")

l.sorted

// there are many ways to build orderings

val byLength: Ordering[String] = new Ordering[String] {
  def compare(x: String, y: String) = x.length - y.length
}

l.sorted(byLength)


val simplerByLength: Ordering[String] = Ordering by (_.length)

l.sorted(simplerByLength)


val yetAnotherByLength: Ordering[String] = Ordering.Int on (_.length)

l.sorted(yetAnotherByLength)


l.sorted(Ordering.String.reverse)