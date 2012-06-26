# Automatic Minesweeper Solver

This software solves Minesweeper games automatically.  Specifically,
it will work with Simon Tatham's *Mines* game on X.  See
[this article](http://www.cmears.id.au/articles/mines-solver) for more
details.

## Building

Run `make` to build.  You'll need GHC, some packages from Cabal, and
Gecode.  To run you'll also need `xdotool`, `xwddump` and
Imagemagick (and *Mines* itself, of course).  If you get two
executables, `Solve` and `mines-solve`, then you're ready to run.

## Running

Run `mines` and make sure its window is visible, then run `./Solve`.
Make sure the `mines` window is sufficiently large --- the shadows of
the grid cells should be clearly visible.
