module Board where

import Data.Matrix

data Move = X | O deriving (Eq,Show,Read)
type Cell = Maybe Move
type Board = Matrix Cell
type Pos = (Int,Int)


rows :: Board -> [[(Pos,Cell)]]
rows = toLists . mapPos (,)

emptyBoardOfSize :: Int -> Board
emptyBoardOfSize n = matrix n n $ const Nothing

emptyBoard :: Board
emptyBoard = emptyBoardOfSize 3

nextMove :: Move -> Move
nextMove X = O
nextMove O = X

putOn :: Cell -> Pos -> Board -> Maybe Board
putOn = safeSet

moveTo :: Move -> Pos -> Board -> Maybe Board
moveTo m (i,j) b = case safeGet i j b of
    Nothing -> Nothing  -- invalid index
    Just (Just _) -> Nothing  -- cell isn't empty
    _ -> putOn (Just m) (i,j) b

isFull :: Board -> Bool
isFull = notElem Nothing

wins :: Move -> Board -> Bool
wins m b = any (any isFullSeries) [cols,rows_,diags]
    where
        cols = map (`getCol` b) [1 .. ncols b]
        rows_ = map (`getRow` b) [1 .. nrows b]
        diags = [getDiag b, getAntiDiag b]
        getAntiDiag = getDiag . fromLists . reverse . toLists
        isFullSeries = all (== Just m)

anyWins :: Board -> Bool
anyWins b = wins X b || wins O b
