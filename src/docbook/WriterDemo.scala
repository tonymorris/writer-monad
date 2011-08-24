/*
$ scala WriterDemo 41
Result: false
LOG
===
starting with 41
adding 7
switcheroo with 441
is even?
*/
object WriterDemo {
  import Writer._

  def main(args: Array[String]) {
    val k = args(0).toInt

    val r =
      for {
        a <- k withvaluelog ("starting with " + _)
        b <- (a + 7) withlog "adding 7"
        c <- (b * 3).nolog
        d <- c.toString.reverse.toInt withvaluelog ("switcheroo with " + _)
        e <- (d % 2 == 0) withlog "is even?"
      } yield e

    println("Result: " + r.a)
    println("LOG")
    println("===")
    r.log foreach println
  }
}
