package sudoku

class Entry (s : Set[Int] = Set.range(1,10)) {

  def remove(x : Int): Entry = {
    new Entry(s - x)
  }

  def has(x : Int): Boolean = {
    s.contains(x)
  }

  def empty(): Boolean = {
    s.isEmpty
  }

  def remaining() : Set[Int] = s

  def size() : Int = {
    s.size
  }

  def unique() : Int = {
    if (s.size == 1) {
      s.head
    }
    else {
      0
    }
  }
}

object Entry {
  def only(x : Int): Entry = {
    new Entry(Set[Int]() + x)
  }
}
