package sudoku
import scala.io.Source

object Main extends App {
  val fname = "/Users/davidcashman/dev/sudoku/top95.txt"
  var total_time = 0L
  val t00 = System.nanoTime()
  val f = Source.fromFile(fname)
  for (line <- f.getLines) {
    val t0 = System.nanoTime()
    val x = Board.fromString(line)
    total_time += System.nanoTime() - t0
    x match {
      case None => println("Bad initial board :(")
      case Some(b) =>
        val t1 = System.nanoTime()
        val result = b.solve()
        total_time += System.nanoTime() - t1
        result match {
          case None => println("No solution found :(")
          case Some(b) =>
            println(b)
        }
    }
  }
  f.close()
  println(total_time * 1.0e9)
  println((System.nanoTime() - t00) * 1.0e9)
}
