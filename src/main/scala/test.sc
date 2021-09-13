import scala.collection.immutable.HashMap

val s1 = Seq( "lo*g", "*ong", "lon*", "l*ng" )

def hoodOrder( s1: String, s2: String ):Boolean = {
  s1.indexOf('*') < s2.indexOf('*')
}

s1.sortWith(hoodOrder)

val s2 = List( "hello", "goodbye", "fog", "bee", "apple" )

@scala.annotation.tailrec
def order(s1: String, s2: String ):Boolean = {
  s1.charAt(0).toLower < s2.charAt(0).toLower && order( s1.substring(1), s2.substring(1))
}

//s2.sortWith(order)

val actual = HashMap("mat" -> List("cat", "man"), "mix" -> List(), "ten" -> List("men", "tan"), "men" -> List("man", "ten"), "man" -> List("men", "mat", "tan"), "tan" -> List("man", "ten"), "cat" -> List("mat"))
val e = for( k <- actual.keySet ) yield ( k, actual(k).sortWith(order))

actual == e.toMap

val test = List("men", "mat", "tan")
test.sorted