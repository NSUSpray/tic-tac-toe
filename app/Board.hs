module Board where

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

moveAfter :: Move -> Move
moveAfter X = O
moveAfter O = X

putOn :: Cell -> Pos -> Board -> Maybe Board
putOn = safeSet

movedTo :: Move -> Pos -> Board -> Maybe Board
movedTo m (i,j) b = case safeGet i j b of
    Nothing -> Nothing  -- invalid index
    Just (Just _) -> Nothing  -- cell isn't empty
    _ -> Just m `putOn` (i,j) $ b

isFull :: Board -> Bool
isFull = notElem Nothing

winsOn :: Move -> Board -> Bool
m `winsOn` b = any (any isFullSeries) [cols,rows_,diags]
    where
        cols = map (`getCol` b) [1 .. ncols b]
        rows_ = map (`getRow` b) [1 .. nrows b]
        diags = [getDiag b, getAntiDiag b]
        getAntiDiag = getDiag . fromLists . reverse . toLists
        isFullSeries = all (== Just m)

anyWinsOn :: Board -> Bool
anyWinsOn b = X `winsOn` b || O `winsOn` b
