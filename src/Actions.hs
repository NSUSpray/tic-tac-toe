{-# LANGUAGE OverloadedStrings #-}

module Actions (movingResponse,processMove) where

import Text.Read (readMaybe)

import Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S

import Board
import qualified Html


response :: String -> Board -> S.ActionM ()
response msg = S.html . renderHtml . Html.template msg

movingResponse :: Board -> S.ActionM ()
movingResponse = response "Move of {}:"


readFormParams :: S.ActionM (Board,Pos)
readFormParams = do
    cellsStr <- S.formParam "board"
    posStr <- S.formParam "pos"
    case do
        cells <- readMaybe cellsStr
        pos <- readMaybe posStr
        return (fromLists cells, pos)
        of
        Nothing -> do
            response
                "Something went wrong. You’ll have to start over :(" emptyBoard
            S.finish
        Just res -> return res

processMove :: S.ActionM ()
processMove = do
    (currentBoard,newPos) <- readFormParams
    let currentPlayer = nextMoveOn currentBoard
    case currentPlayer `movedTo` newPos $ currentBoard of
        Nothing -> ($ currentBoard) $
            response
                "This cell is already occupied. {}, please choose another:"
        Just newBoard -> ($ newBoard) $
            if currentPlayer `winsOn` newBoard then
                response "{} wins!"
            else if ($ newBoard) isFull then
                response "You played a draw."
            else
                movingResponse
