package sudoku

import scala.collection.mutable

object Board {
  def flat(x : Int, y : Int): Int = x+9*y
  def fromString(s: String): Option[Board] = {
    val b = new Board()
    for (x <- 0 to 8) {
      for (y <- 0 to 8) {
        if (s.charAt(flat(x,y)) != '.') {
          if (!b.try_assign(flat(x, y), s.charAt(x + 9 * y).asDigit)) {
            println(s"Couldn't assign value at $x,$y")
            return None
          }
        }
      }
    }
    Some(b)
  }
}

class Board {
  var b : Array[Entry] = Array.fill[Entry](81)(new Entry)

  def at(x : Int, y : Int) : Entry = {
    b(Board.flat(x,y))
  }

  def solved() : Boolean = {
    b.forall(_.unique() != 0)
  }

  def solve() : Option[Board] = {
    if (solved()) {
      return Some(this)
    }
    // Find the candidate with fewest remaining choices
    val best_pos =
      b.zipWithIndex.filter(_._1.size() > 1).minBy(_._1.size())._2

    for (i <- b(best_pos).remaining()) {
      var newboard = new Board()
      // Hmm, this is the only reason I need b to be a var.
      // Could do element-wise copy.  Or have multiple constructors?
      newboard.b = b.clone()
      if (newboard.try_assign(best_pos, i)) {
        val solution = newboard.solve()
        solution match {
          case Some(board) => return solution
          case None =>
        }
      }
    }
    // No satisfying solution
    None
  }

  // Return the three groups - row, column, and box
  def neighbour_groups (pos : Int): mutable.Set[mutable.Set[Int]] = {
    val x = pos % 9
    val y = pos / 9

    val s: mutable.Set[mutable.Set[Int]] =
      mutable.Set(mutable.Set.range(x, x + 81, 9),
        mutable.Set.range(y * 9, y * 9 + 9))

    // Ugh.  The little 3x3 box
    val box: mutable.Set[Int] = mutable.Set()
    val xlow = x - x % 3
    val ylow = y - y % 3
    for (i <- xlow to xlow + 2) {
      for (j <- ylow to ylow + 2) {
        box.add(Board.flat(i, j))
      }
    }
    s.add(box)
    s
  }

  def neighbours (pos : Int): mutable.Set[Int] = {
    neighbour_groups(pos).flatten.diff(mutable.Set(pos))
  }

  // Remove v as a possibility at pos.
  // If the resulting board is illegal, return false.
  def eliminate(pos : Int, v : Int) : Boolean = {
    if (!b(pos).has(v)) {
      // Already removed
      return true
    }
    b(pos) = b(pos).remove(v)
    if (b(pos).empty()) { return false }
    else if (b(pos).unique() > 0) {
      // Down to one
      for (otherpos <- neighbours(pos)) {
        if (!eliminate(otherpos, b(pos).unique())) {
          return false
        }
      }
    }

    for (group <- neighbour_groups(pos)) {
      var n = 0
      var pos_found = 0
      for (pos <- group) {
        if (b(pos).has(v)) {
          n += 1
          pos_found = pos
        }
      }
      if (n == 0) {
        // There are no legal positions for v
        return false
      }
      else if (n == 1) {
        if (!try_assign(pos_found, v)) {
          return false
        }
      }
    }
    true
  }

  // Try to assign a value, returning true if we succeed.
  // Everything that happens here is deterministic, so
  // we don't need to copy the board as we eliminate.
  def try_assign(pos : Int, v : Int) : Boolean = {
    for (i <- 1 to 9) {
      if (i != v && !eliminate(pos, i)) {
        return false
      }
    }
    true
  }

  override def toString: String = {
    val s = new StringBuilder("")
    for (row <- 0 to 8) {
      if (row == 3 || row == 6) {
        s ++= "---+---+---\n"
      }
      for (column <- 0 to 8) {
        val e : Int = at(column,row).unique()
        if (column == 3 || column == 6) {
          s ++= "|"
        }
        s ++= (if (e == 0) "." else e.toString)
      }
      s ++= "\n"
    }
    s.toString
  }
}

