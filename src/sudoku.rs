use std::fmt;

#[derive(Debug, Copy, Clone)]
struct Entry {
   // Indicates whether the given value is possible.
   // Unfortunately offset by 1 relative to user sudoku values.
   e : [bool; 9]
}

impl Entry { 

   fn count(&self) -> u8 {
      return self.e.iter().fold(0, |sum, x| sum + (if *x { 1 } else { 0 }))
   }
   
   // Return the unique entry, if there is one.  Would be a bit more efficient
   // to fold this into count.
   fn unique(&self) -> Option<u8> {
      if self.count() == 1 {
         for i in 0..9 {
            if self.e[i] {
               return Some(i as u8);
            }
         }
      }
      None
   }
}

impl fmt::Display for Entry {
   fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      if let Some(v) = self.unique() {
         write!(f, "{}", v+1)
      }
      else {
         write!(f, ".")
      }
   }
}

// #[derive(Debug, Copy, Clone)]
pub struct Board {
   // Just represent a board with a flat vector.  
   // Maybe 2x2 would be better?
   v : [Entry ; 81]
}

impl Board {

   // Create an empty board.  Any value is possible
   pub fn new() -> Board {
      Board {
         v: [Entry{e : [true; 9]}; 81]
      }
   }

   // We've solved it if every space has exactly one
   // possible value.
   fn solved(&self) -> bool {
      for entry in self.v.into_iter() {
         if entry.count() != 1 {
            return false;
         }
      }
      return true;
   }

   // Assign value at pos, and return true if assignment was successful.
   // Note that board is not restored on failure.
   fn try_assign(&mut self, pos : usize, val : usize) -> bool {
      // Just eliminate all other values
      for i in 0..9 {
         if i != val && !self.eliminate(pos, i) {
            return false;
         }
      }
      return true;
   }

   fn neighbours(pos : usize) -> Vec<usize> {
      // TODO: Figure out how to cache neighbour information.
      // I guess an array of boxes would be good.  I probably need
      // some sort of manager to share across boards.
      let x = pos % 9;
      let y = pos / 9;
      let three_by_three_xlow = x - (x % 3);
      let three_by_three_ylow = y - (y % 3);
      
      let mut vec = Vec::new();
      
      for i in 0..9 {
         if i != x {
            vec.push(y*9+i);
         }
         if i != y {
            vec.push(i*9+x);
         }
      }
      for i in three_by_three_xlow..(three_by_three_xlow+3) {
         for j in three_by_three_ylow..(three_by_three_ylow+3) {
            // Don't push anything in same row or column, since we already
            // pushed it.
            if i != x && j != y {
               vec.push(j*9+i);
            }
         }
      }
      vec
   }
   
   fn groups_of_pos(pos : usize) -> [Vec<usize>; 3] {
      // TODO: Figure out how to cache groups_of_pos information.
      let mut vecs = [vec![], vec![], vec![]];

      let x = pos % 9;
      let y = pos / 9;
      let three_by_three_xlow = x - (x % 3);
      let three_by_three_ylow = y - (y % 3);

      for i in 0..9 {
         vecs[0].push(y*9+i);
         vecs[1].push(i*9+x);
      }
      // Now the 3x3 grid
      for i in three_by_three_xlow..(three_by_three_xlow+3) {
         for j in three_by_three_ylow..(three_by_three_ylow+3) {
            vecs[2].push(j*9+i);
         }
      }
      return vecs;
   }

   // Remove value val as a possibility at pos.
   // If resulting board is illegal, return false.
   fn eliminate(&mut self, pos : usize, val : usize) -> bool {
      if !self.v[pos].e[val] {
         // Already removed.
         return true;
      }
      self.v[pos].e[val] = false;
      let n = self.v[pos].count();
      if n == 0 {
         return false;
      }
      else if n == 1 {
         // Down to one possibility.  We can
         // eliminate it from all neighbours
         let value = self.v[pos].unique().unwrap();
         for otherpos in Board::neighbours(pos) {
            if !self.eliminate(otherpos, value as usize) {
               return false;
            }
         }
      }

      // Now, check the three groups that pos is in.
      // For each group, if val appears only once
      // in the group, it must go there.
      for group in Board::groups_of_pos(pos).iter() {
         let mut n = 0;
         let mut pos_found = 0;
         for pos in group {
            if self.v[*pos].e[val] {
               n += 1;
               pos_found = *pos;
            }
         }
         if n == 0 {
            return false;
         }
         else if n == 1 {
            // We can assign val at pos_found.
            if !self.try_assign(pos_found, val) {
               return false;
            }
         }
      }
      return true;
   }

   // Recursive search.  Returns a solved board.
   pub fn search(board : &Option<Board>) -> Option<Board> {
      // If initial board was illegal, board may already be None
      match board {
         &None => return None,
         &Some(ref b) => 
            if b.solved() {
               //println!("{}", b);
               return board.clone();
            }
            else {
               //println!("{}", b);
               if let Some(pair) = b.v.iter()
                                          .enumerate()
                                          .filter(|pair| pair.1.count() > 1)
                                          .min_by_key(|pair| pair.1.count()) {
                  let pos = pair.0;
                  let entry = pair.1;
                  for i in 0..9 {
                     if entry.e[i] {
                        let mut newboard = b.clone();
                        if newboard.try_assign(pos, i) {
                              let result = Board::search(&Some(newboard));
                           if result.is_some() {
                              return result;
                           }
                        }
                     }
                  }
                  // No satisfying assignment.
                  return None;
               }
               else {
                  // What causes us to reach here?
                  return None;
               }
            }
      }
   }

   // Create a board from a string.
   pub fn from_string(s : &str) -> Option<Board> {
      let mut b = Board::new();
      let mut i = 0;
      for c in s.chars() {
         let result = if c == '.' { Some(0) } else { c.to_digit(10)} ;
         // Bad input
         if result.is_none() { 
            println!("Couldn't parse: {}", c);
            return None; 
         }
         let n = result.unwrap() as usize;
         
         if n != 0 {
            let result = b.try_assign(i, n-1);
            // Should switch to return Optional<Board>
            assert!(result);
         }
         i += 1;
      }
      Some(b)
   }

   fn at(&self, x : u8, y : u8) -> Entry {
      assert!(x < 9);
      assert!(y < 9);
      let x = x as usize;
      let y = y as usize;
      self.v[y*9 + x]
   }
}

impl fmt::Display for Board {
   fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      let dashes = "------";
      let dash_row = format!("{d}+{d}+{d}", d=dashes);
      for y in 0..9 {
         if y == 3 || y == 6 {
            try!(write!(f, "{}\n", dash_row));
         }
         for x in 0..9 {
            if x == 3 || x == 6 {
               try!(write!(f,"|"));
            }
            try!(write!(f, "{} ", self.at(x, y)));
         }
         try!(write!(f, "\n"));
      }
      write!(f, "")
   }
}

impl Clone for Board {
   fn clone(&self) -> Board {
      return Board {
         v: self.v
      }
   }
   
   fn clone_from(&mut self, _: &Board) {
   }
}

