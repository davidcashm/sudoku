package sudoku
import scala.collection.immutable.BitSet

// Represent an entry as a bit field
class Entry (s : BitSet = BitSet(1,2,3,4,5,6,7,8,9) ) {

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
    new Entry(BitSet() + x)
  }
}
