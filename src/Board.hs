module Board (module Board, getElem, toLists, fromLists) where

import Data.Matrix


data Move = X | O deriving (Eq,Show,Read)
type Cell = Maybe Move
type Board = Matrix Cell
type Pos = (Int,Int)


rowsOf :: Board -> [[(Pos,Cell)]]
rowsOf = toLists . mapPos (,)

emptyBoardOfSize :: Int -> Board
emptyBoardOfSize n = matrix n n $ const Nothing

emptyBoard :: Board
emptyBoard = emptyBoardOfSize 3

nextMoveOn :: Board -> Move
nextMoveOn board = if count X > count O then O else X
    where
        count move = length $ filter (== Just move) $ concat $ toLists board

putOn :: Cell -> Pos -> Board -> Maybe Board
putOn = safeSet

movedTo :: Move -> Pos -> Board -> Maybe Board
movedTo player (i,j) board = case safeGet i j board of
    Nothing -> Nothing  -- invalid index
    Just (Just _) -> Nothing  -- cell isn't empty
    Just Nothing -> Just player `putOn` (i,j) $ board

isFull :: Board -> Bool
isFull = notElem Nothing

winsOn :: Move -> Board -> Bool
player `winsOn` board = any (any isFullSeries) [cols,rows,diags]
    where
        cols = map (`getCol` board) [1 .. ncols board]
        rows = map (`getRow` board) [1 .. nrows board]
        diags = [getDiag board, getAntiDiag board]
        getAntiDiag = getDiag . fromLists . reverse . toLists
        isFullSeries = all (== Just player)

anyWinsOn :: Board -> Bool
anyWinsOn board = X `winsOn` board || O `winsOn` board
