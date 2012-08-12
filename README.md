minesweeper-sweeper
===================

Minesweeper board solver written in Haskell.

In the future (after finishing the optimisation and fixing everything else), I also
feel like enhancing it (with a Python script, maybe) to operate on the actual
Minesweeper game, although that would be more difficult (screen recognition/
tinkering with API).

Usage
-----

It doesn't exactly have an interface yet. Use `parseBoard` to convert a string of
characters to a list of cell states ("." - unrevealed, "F" - flagged, digit -
revealed with a number of mines (0 for empty cell)), then `listArray (startcoords,
endcoords) $ parseBoard` to create the array. Then feed it to `solveBoard`.

How does it work
----------------

`solveBoard` applies `backtrackCell` to every empty cell on the board. 
`backtrackCell`sets the cell to either having or not having a mine and calls
`analyzeBoard` on every variant. If both variants are valid, we cannot know for
sure whether the cell is dangerous. If both are invalid, whatever assumption
brought us to this position is invalid. Otherwise, the not-invalid variant has to
be valid and whatever results we got from making that assumption are returned. It
also keeps track of what cells we've visited to avoid working on them twice.

`analyzeCells` calls `analyzeCell` on every revealed cell on the board that has
unknown neighbours. It tries to find out whether the cell has some neighbours that
surely have a mine or are empty. It also detects contradictions (e.g. more mines
around the cell than there are unrevealed cells) that would then get signalled back
to `backtrackCell`. `analyzeCells` then concatenates everything that `analyzeCell`
inferred about every cell (if there was a contradiction, `analyzeCells` returns
`Nothing`).

`analyzeBoard` repeatedly calls `analyzeCells`, gets its conclusions and applies
them to the game board until `analyzeCells` cannot infer anything else about the
board (this should be the time when `backtrackCell` is called again to get some
more information`).

Known problems
--------------

I have just realised that `backtrackCell` doesn't quite call itself, so there is
no backtracking. Also:

* Some weird stuff going on with packing and unpacking things into `Maybe`.
* The code is a bit rough and has some operations it would be useful to generalise.
* Something else.
