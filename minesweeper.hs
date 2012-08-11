--Reasons about the positions of the mines and safe cells on a Minesweeper board.
--Cannot solve ambiguous boards so far.

import Data.Array
import Data.List (nub, foldr1)
import Data.Maybe (catMaybes, isNothing, fromJust)
import Control.Applicative (liftA2)

--A position of in the grid (row, column)
type Position = (Integer, Integer)

--What's in the cell?
data CellState = Unknown           --Unrevealed
               | AssumedMine       --Unrevealed, inferred dangerous
               | AssumedSafe       --Unrevealed, inferred safe
               | Revealed Int      --Revealed, number of mines around it
               deriving (Show, Eq)

--The playing field
type GameBoard = Array Position CellState

--Turns a string of characters into a board
parseBoard :: String -> [CellState]
parseBoard = map parseSymbol

parseSymbol :: Char -> CellState
parseSymbol c
    | c == '.' = Unknown
    | c == 'F' = AssumedMine 
    | otherwise = Revealed $ read [c]

--An unambiguous board 
testBoard = listArray ((0,0), (7,7)) $ parseBoard $
    "........" ++
    "2211...." ++
    "000112.." ++
    "0000013." ++
    "0000002." ++
    "0011113." ++
    "001....." ++
    "001....."

testBoard2 = listArray ((0,0), (15, 15)) $ parseBoard $ 
    "................" ++
    "................" ++
    "................" ++
    ".......1112....." ++
    "....2111001....." ++
    "....2000002....." ++
    "....2110001....." ++
    "......100012...." ++
    "12....111101...." ++
    "01.......112...." ++
    "01.............." ++
    "01221112........" ++
    "000000012......." ++
    "111000002......." ++
    "..1111002......." ++
    ".....1002......."

--Ambiguous board, we have to make assumptions and check for contradictions to
--proceed.
testBoardComplex = listArray ((0,0), (7,7)) $ parseBoard $
    "001F1111" ++
    "111111F1" ++
    "F1000111" ++
    "11111000" ++
    "113F3100" ++
    "...FF221" ++
    "........" ++
    "........"

--A conclusion about an unrevealed position
data Conclusion = Mine Position
                | Safe Position 
                deriving (Eq, Show)

--Lists the coordinates of the neighbours of a cell
getNeighbours :: GameBoard -> Position -> [Position]
getNeighbours board (x, y) = 
    filter (/= (x,y)) [(row, col) | 
        row <- [minrow..maxrow],
        col <- [mincol..maxcol]]
    where maxrow = min (x+1) $ fst $ snd $ bounds board
          minrow = max (x-1) 0
          maxcol = min (y+1) $ snd $ snd $ bounds board
          mincol = max (y-1) 0

--Filters coordinates from a list by the type of the cells they contain
filterType :: CellState -> GameBoard -> [Position] -> [Position]
filterType cellType board = filter (\pos -> board ! pos == cellType)

--Is this cell revealed?
isRevealed :: GameBoard -> Position -> Bool
isRevealed board pos =
    case board ! pos of
        Revealed _ -> True
        otherwise  -> False

--Reasons about a single cell:
--  * If the number of flags around it is the same as the
--    number of mines, all unknown cells around it are safe.
--  * If the number of unrevealed + flagged cells is the same as the
--    number of mines, all unknown cells are trapped.
--  * If the number of flags around the cell is greater than the
--    number of mines or the number of unknown + flagged cells is
--    smaller than it, the board is invalid.
analyzeCell :: GameBoard -> Position -> Maybe [Conclusion]
analyzeCell board pos =
    case board ! pos of
        Unknown     -> Just []
        AssumedMine -> Just []
        AssumedSafe -> Just []
        Revealed noMines 
            | noMineCells == noMines 
                -> Just $ map Safe unknownNeighbours
            | noUnknownCells + noMineCells == noMines 
                -> Just $ map Mine unknownNeighbours
            | noMineCells > noMines -> Nothing
            | noUnknownCells + noMineCells < noMines -> Nothing
            | otherwise -> Just []
        where
            neighbours = getNeighbours board pos
            mineNeighbours = filterType AssumedMine board neighbours
            unknownNeighbours = filterType Unknown board neighbours
            safeNeighbours = filterType AssumedSafe board neighbours
            noMineCells = length mineNeighbours
            noUnknownCells = length unknownNeighbours
            noSafeCells = length safeNeighbours

--Executes analyzeCell on all revealed cells in the board and concatenates
--the answers, removing duplicates.
analyzeBoard :: GameBoard -> Maybe [Conclusion]
analyzeBoard board
    | any isNothing resultMaybes = Nothing
    | otherwise = Just $ nub . concat $ catMaybes resultMaybes
    where
        revealedCells = filter (isRevealed board) $ range $ bounds board
        resultMaybes = map (analyzeCell board) revealedCells

--Records all conclusions on the game board
noteAnalysis :: [Conclusion] -> GameBoard -> GameBoard
noteAnalysis conclusions oldBoard = 
    oldBoard // map convert conclusions 
    where
        convert :: Conclusion -> (Position, CellState)
        convert (Mine pos) = (pos, AssumedMine)
        convert (Safe pos) = (pos, AssumedSafe)

--Repeatedly analyzes the board and concatenates all
--conclusions until nothing else can be inferred about the board
analyzeShallow :: GameBoard -> Maybe [Conclusion]
analyzeShallow board = case analysis of
    Nothing -> Nothing
    Just [] -> Just []
    otherwise -> combineMaybe analysis $ 
        analyzeShallow (noteAnalysis (fromJust analysis) board)
    where
        analysis = analyzeBoard board
        combineMaybe a b = 
            if (isNothing a || isNothing b) 
                then Nothing
                else liftA2 (++) a b 

--Performs a deep analysis of an unknown cell by marking it dangerous
--or safe and checking for contradictions further on.
--If a cell is marked safe and it results in a contradiction, the cell
--must be dangerous and vice versa.
deepAnalyzeCell :: GameBoard -> Position -> Maybe [Conclusion]
deepAnalyzeCell board pos
    | (isNothing withMineResult) && (isNothing withoutMineResult) = Nothing
    | isNothing withMineResult = liftA2 (++) (Just [Safe pos]) withoutMineResult
    | isNothing withoutMineResult = liftA2 (++) (Just [Mine pos]) withMineResult
    | otherwise = Just []
    where
        withMineResult    = analyzeShallow $ board // [(pos, AssumedMine)]
        withoutMineResult = analyzeShallow $ board // [(pos, AssumedSafe)]

--Executes deepAnalyzeCell on all unknown cells in the board and concatenates
--the answers.
deepAnalyzeBoard :: GameBoard -> [Conclusion]
deepAnalyzeBoard board = nub . concat $ 
    catMaybes $ map (deepAnalyzeCell board) unknownNeighbouredCells
    where
        unknownNeighbouredCells = 
            filter (\pos -> any (isRevealed board) (getNeighbours board pos))
                $ filterType Unknown board $ range $ bounds board

