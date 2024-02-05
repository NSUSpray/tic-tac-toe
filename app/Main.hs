{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Text.Read (readMaybe)

import Data.Matrix (fromLists,toLists)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (form,title)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S
import Network.Wai.Middleware.Static

import Board


cellToHtml :: Move -> Bool -> (Pos,Cell) -> Html
{- not win and empty cell -}
cellToHtml possibleMove False (ij,Nothing) =
    button ! name "pos" ! value tValue $
        toHtml $ show possibleMove
    where tValue = toValue $ show ij
{- non-empty cell -}
cellToHtml _ _ (_, Just madeMove) = toHtml $ show madeMove
{- win -}
cellToHtml _ True _ = text " "

boardToHtml :: Move -> Board -> Html
boardToHtml move board = form ! method "post" $ do
    move `storeAs` "move"
    toLists board `storeAs` "board"
    table $ tbody $ forM_ (rowsOf board) $ tr . mapM_ tdFromCell
    where
        tValue `storeAs` tName = input
            ! type_ "hidden"
            ! name tName
            ! value (toValue $ show tValue)
        tdFromCell = td . cellToHtml move (anyWinsOn board)


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
    tBoard <- S.formParam "board"
    tMove <- S.formParam "move"
    tPos <- S.formParam "pos"
    case do
        board <- readMaybe tBoard
        move <- readMaybe tMove
        pos <- readMaybe tPos
        return (fromLists board, move, pos)
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


main :: IO ()
main = S.scotty port $ do
    S.middleware $ staticPolicy (noDots >-> addBase "static")
    S.get "/" $ movingResponse X emptyBoard
    S.post "/" processRequest
    where port = 3000
