{-# LANGUAGE OverloadedStrings #-}

module Actions (movingResponse,processMove) where

import Text.Read (readMaybe)

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (form,title)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S

import Board
import qualified Html


response :: String -> Board -> S.ActionM ()
response msg board = S.html $ renderHtml $ docTypeHtml $ do
    H.head $ do
        title "Tic-tac-toe"
        link ! rel "stylesheet" ! href "/main.css"
        link ! rel "icon" ! href "/favicon.png" ! type_ "image/png"
    body $ do
        h1 $ toHtml $ subsMove msg
        Html.fromBoard move board
        form $ p $ button "Restart"
    where
        subsMove ('{':'}':xs) = "‘" ++ show move ++ "’" ++ xs
        subsMove (x:xs) = x : subsMove xs
        subsMove "" = ""
        move = (if anyWinsOn board then lastMoveOn else nextMoveOn) board

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
