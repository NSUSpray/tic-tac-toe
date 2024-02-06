{-# LANGUAGE OverloadedStrings #-}

module Actions (movingResponse,processRequest) where

import Text.Read (readMaybe)

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (form,title)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S

import Board
import BoardToHtml


response :: String -> Move -> Board -> S.ActionM ()
response msg move board = S.html $ renderHtml $ docTypeHtml $ do
    H.head $ do
        title "Tic-tac-toe"
        link ! rel "stylesheet" ! href "/main.css"
        link ! rel "icon" ! href "/favicon.png" ! type_ "image/png"
    body $ do
        h1 $ toHtml $ subsMove msg
        boardToHtml move board
        form $ p $ button "Restart"
    where
        subsMove ('{':'}':xs) = "‘" ++ show move ++ "’" ++ xs
        subsMove (x:xs) = x : subsMove xs
        subsMove "" = ""

movingResponse :: Move -> Board -> S.ActionM ()
movingResponse = response "Move of {}:"


readFormParams :: S.ActionM (Board,Move,Pos)
readFormParams = do
    sCells <- S.formParam "board"
    sMove <- S.formParam "move"
    sPos <- S.formParam "pos"
    case do
        cells <- readMaybe sCells
        move <- readMaybe sMove
        pos <- readMaybe sPos
        return (fromLists cells, move, pos)
        of
        Nothing -> do
            response "Something went wrong. You’ll have to start over :("
                X emptyBoard
            S.finish
        Just res -> return res

processRequest :: S.ActionM ()
processRequest = do
    (currentBoard,currentPlayer,newPos) <- readFormParams
    case currentPlayer `movedTo` newPos $ currentBoard of
        Nothing -> ($ currentBoard) $
            response
                "This cell is already occupied. {}, please choose another:"
                currentPlayer
        Just newBoard -> ($ newBoard) $
            if currentPlayer `winsOn` newBoard then
                response "{} wins!" currentPlayer
            else if ($ newBoard) isFull then
                response "You played a draw." X
            else
                movingResponse (moveAfter currentPlayer)
