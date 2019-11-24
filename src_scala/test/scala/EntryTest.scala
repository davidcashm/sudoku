import org.scalatest.FunSuite
import sudoku.Entry

class EntryTest extends FunSuite{
  test("EntryTest.has") {
    val x = Entry.only(1)
    assert(x.has(1))
    assert(!x.has(2))
    assert(x.unique() == 1)
  }

  test("EntryTest.remove") {
    val x = Entry.only(1)
    assert(x.remove(1).empty())
    // Immutable
    assert(x.has(1))
  }

  test("EntryTest.defaultInit") {
    val x = new Entry()
    assert(x.has(1))
    assert(x.has(9))
  }
}
