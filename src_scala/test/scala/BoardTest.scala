import org.scalatest.FunSuite
import sudoku.Board

class BoardTest extends FunSuite {
  test("BoardTest.solved") {
    // TODO: Better tests
    val x = Board.fromString("..84...3....3.....9....157479...8........7..514.....2...9.6...2.5....4......9..56")
    val y = Board.fromString("123456789123456789123456789123456789123456789123456789123456789123456789123456789")
    assert(y.isEmpty)
  }
}
