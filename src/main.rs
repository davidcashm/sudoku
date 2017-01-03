mod sudoku;
extern crate time;
use std::io::BufReader;
use std::io::BufRead;
use std::fs::File;

#[cfg(test)]
mod tests {
   #[test]
   fn it_works() {
      println!("Hello World");
   }
}

fn main() {
// Makes a blank board
//   let b = sudoku::Board::new();

// Make a board with values in it
//   let p = "003020600900305001001806400008102900700000008006708200002609500800203009005010300";
//   let p = "000000000900305001001806400008102900700000008006708200002609500800203009005010300";
//   let b = sudoku::Board::from_string(p);
//   println!("{}", b);
//   if let Some(bf) = sudoku::Board::search(&Some(b)) {
//      println!("{}", bf);
//   }
//   else {
//      println!("No solution");
//   }
   let f = File::open("top95.txt").unwrap();
   let file = BufReader::new(&f);
   let mut total = 0.0;
   for line in file.lines() {
      let l = line.unwrap();
      let start = time::precise_time_s();
for _ in 0..1 {
      let b = sudoku::Board::from_string(&l);
      let mut delta = time::precise_time_s() - start;
      
      if b.is_none() {
         println!("Couldn't parse: {}", l);
      }
      else {
         let start = time::precise_time_s();
         let result = sudoku::Board::search(&b);
         delta += time::precise_time_s() - start;
         if let Some(bf) = result {
            println!("{}", b.unwrap());
            println!("{}", bf);
         }
         else {
            println!("No solution");
         }
      }
      total += delta;
      println!("Time: {}", delta);
}
   }
   println!("Total time: {}", total);
}

