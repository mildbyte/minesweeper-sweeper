minesweeper-sweeper
===================

Minesweeper board solver written in Haskell. It also controls the actual GNOME 3
Mines program by using a magic mixture of Python, makeshift image recognition,
various X APIs and pure luck.


Usage
-----

***The solver***

Launch the compiled executable (or the main function), enter the width and the
height of the field (separated with a new line), then width * height characters
representing the state of each cell ("." - unrevealed, "F" - flagged, digit -
revealed with a number of mines (0 for empty cell)). You will get a list of
positions of mines and safe cells.

***The harness***

The harness only works with Python 2. You also need to have:
* GTK bindings
* LibWnck
* Xlib

On an ArchLinux system, you can install the needed packages with 
`pacman -S python-wnck pygtk python-xlib`. I am not sure about other distributions.

The usage is as follows:
* Launch gnomine, open the board and make sure the window is completely inside of
  the screen.
* Launch the harness. It will ask you for the number of cells in a row and recognize
  the position of the grid.
* The harness then will solve your board for you (not completely, see Issues),
  sometimes asking you for the contents of the cells it did not recognize

How does it work
----------------

***The solver***

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

***The harness***

In the beginning, the harness takes a picture of the Mines window and finds the
boundaries of the grid on it by moving towards the middle from each side of the
image until the colour changes. It then (by asking for the number of cells in a row)
calculates the side of a cell. At this stage, we can map a coordinate of a cell in
the game world onto a square in the actual screenshot.

The image recognition happens by taking the average colour of the pixels belonging
to a cell (these colours vary quite noticeably among cells with different contents)
and finding the closest colour to the calculated one in a dictionary (the distance
between colours is basic Cartesian). If the resultant distance is smaller than the
tolerance, the contents of the cell have been successfully recognized, otherwise,
the user is asked to specify the contents of the cell.

The harness performs image recognition on all cells and forms the input for the
Haskell solver. It then launches it, gets the result, parses it and marks the
relevant cells. The cycle is repeated again until the Haskell program has nothing
to output.

Known problems
--------------

* There is no backtracking: `backtrackCell` never calls itself again, so the program
  can't solve all boards (and can't even solve some obvious ones).
* The image recognition is quite slow, taking ~1 second (there is also a delay of 0.5
  seconds in the screenshot routine between activating the window and taking the
  screenshot to ensure the window has been redrawn and is in the foreground).
* It doesn't recognise when the board has been solved and so even tries to recognise
  the cells on the high score screen.
* Some weird stuff going on with packing and unpacking things into `Maybe`.
* The code is a bit rough and has some operations it would be useful to generalise.
* Something else.
