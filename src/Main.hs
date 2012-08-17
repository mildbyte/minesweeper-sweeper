--Reasons about the positions of the mines and safe cells on a Minesweeper board.

import Data.Array
import Data.List (nub, foldr1, foldl', intersperse)
import Data.Maybe (catMaybes, isNothing, fromJust)
import Control.Applicative (liftA2)
import qualified Data.Set as S

--A position in the grid (row, column)
type Position = (Int, Int)

--What's in the cell?
data CellState = Unknown           --Unrevealed
               | AssumedMine       --Unrevealed, inferred dangerous
               | AssumedSafe       --Unrevealed, inferred safe
               | Revealed Int      --Revealed, number of mines around it
               deriving (Show, Eq)

--The playing field
type GameBoard = Array Position CellState

--Turns a string of characters into a board.
--I should really make it an instance of Read
parseBoard :: String -> [CellState]
parseBoard = map parseSymbol

parseSymbol :: Char -> CellState
parseSymbol c
    | c == '.' = Unknown
    | c == 'F' = AssumedMine 
    | otherwise = Revealed $ read [c]

--Some boards for testing purposes.
testBoard :: GameBoard
testBoard = listArray ((0,0), (7,7)) $ parseBoard $
    "........" ++
    "2211...." ++
    "000112.." ++
    "0000013." ++
    "0000002." ++
    "0011113." ++
    "001....." ++
    "001....."

testBoard2 :: GameBoard
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

--A conclusion about an unrevealed position
data Conclusion = Mine Position
                | Safe Position 
                deriving (Eq, Show, Ord)

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

--Is the cell unknown?
isUnknown :: GameBoard -> Position -> Bool
isUnknown board pos =
    case board ! pos of
        Unknown    -> True
        otherwise -> False

combineMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
combineMaybe a b = if (isNothing a || isNothing b) 
                   then Nothing
                   else liftA2 (++) a b 

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

--Executes analyzeCell on all revealed cells with unknown neighbours and concatenates
--the answers, removing duplicates.
analyzeCells :: GameBoard -> Maybe [Conclusion]
analyzeCells board
    | any isNothing resultMaybes = Nothing
    | otherwise = Just $ nub . concat $ catMaybes resultMaybes
    where
        revealedNeighbouredCells = 
            filter (\pos -> any (isUnknown board) $ getNeighbours board pos) 
            $ filter (isRevealed board) $ range $ bounds board
        resultMaybes = map (analyzeCell board) revealedNeighbouredCells

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
analyzeBoard :: GameBoard -> Maybe [Conclusion]
analyzeBoard board = case analysis of
    Nothing -> Nothing
    Just [] -> Just []
    otherwise -> combineMaybe analysis $ 
        analyzeBoard (noteAnalysis (fromJust analysis) board)
    where
        analysis = analyzeCells board

--Performs a deep analysis of an unknown cell by marking it dangerous
--or safe and checking for contradictions further on.
--If a cell is marked safe and it results in a contradiction, the cell
--must be dangerous and vice versa.
backtrackCell :: GameBoard -> Position 
              -> S.Set Conclusion -> Maybe (S.Set Conclusion)
backtrackCell board pos oldSet
    | (isNothing withMineResult) && (isNothing withoutMineResult) = Nothing
    | isNothing withMineResult = fmap (S.insert $ Safe pos) withoutMineResult
    | isNothing withoutMineResult = fmap (S.insert $ Mine pos) withMineResult
    | otherwise = Just S.empty
    where
        withMineResult    = solveBoard (board // [(pos, AssumedMine)]) oldSet
        withoutMineResult = solveBoard (board // [(pos, AssumedSafe)]) oldSet

--Executes backtrackCell on all unknown cells near the revealed ones
--and concatenates the answers.
--Uses a set to ensure cells aren't inspected twice.
backtrackBoard :: GameBoard -> S.Set Conclusion -> Maybe (S.Set Conclusion)
backtrackBoard board startSet 
    = foldl' foldingFunction (return startSet) unknownNeighbouredCells 
    where
        unknownNeighbouredCells = 
            filter (\pos -> any (isRevealed board) (getNeighbours board pos))
                $ filterType Unknown board $ range $ bounds board

        --If the inspected position is already in the set, return the original set,
        --otherwise, inspect it and append the results to the set.
        foldingFunction :: Maybe (S.Set Conclusion)
                        -> Position
                        -> Maybe (S.Set Conclusion)
        foldingFunction Nothing _ = Nothing
        foldingFunction (Just oldSet) pos
            | (S.member (Safe pos) oldSet) || (S.member (Mine pos) oldSet) 
              = Just oldSet
            | otherwise = case resultSet of
                  Nothing  -> Nothing
                  Just newSet -> Just $ S.union oldSet newSet
            where        
                resultSet = backtrackCell board pos oldSet

        unpackMaybeList :: Maybe [a] -> [a]
        unpackMaybeList Nothing   = []
        unpackMaybeList (Just xs) = xs

--First do the normal (non-backtracking) analysis until there is nothing to infer
--Then do the backtracking
solveBoard :: GameBoard -> S.Set Conclusion -> Maybe (S.Set Conclusion)
solveBoard board startSet = case analyzeBoard board of
    Nothing                -> Nothing
    Just normalConclusions -> if null normalConclusions
                              then backtrackBoard board startSet
                              else Just $ S.fromList normalConclusions

solveBoardWrapper :: GameBoard -> [Conclusion]
solveBoardWrapper board = case solveBoard board S.empty of
    Nothing -> []
    Just cs -> S.toList cs

readInt :: IO Int
readInt = fmap read getLine

inputBoard :: IO GameBoard
inputBoard = do
    boardWidth <- readInt 
    boardHeight <- readInt 
    boardList <- fmap 
        (take (boardWidth * boardHeight) . filter (flip elem "012345678.F"))
        getContents
    return $ listArray ((0, 0), (boardWidth - 1, boardHeight - 1)) $ parseBoard boardList

--Formats the conclusions into a nice string.
showConclusions :: [Conclusion] -> String
showConclusions = concat . intersperse "\n" . map show

--Inputs the board and solves it
main :: IO ()
main = putStr =<< fmap (showConclusions . solveBoardWrapper) inputBoard
