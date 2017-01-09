Rust implementation of a sudoku solver, based on http://norvig.com/sudoku.html, as well as the C++ implementation linked on that page.

Initial implementation:

Debug: 2.2 s total for all "hard" puzzles
Release: 0.23 s total 
Python: 2.9 s total
C++: 0.15 s total, optimization level -O3 (Mac OS compiler - Apple LLVM version 6.0 (clang-600.0.57) (based on LLVM 3.5svn)
Haskell: 1.195 seconds.  Optimization -O2.  Using only immutable data structures.
Ruby: 10.3 seconds
