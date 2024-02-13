{-# LANGUAGE OverloadedStrings #-}

module Html (template) where

import Control.Monad (forM_)

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (form,title)

import Board


fromCell :: Move -> Bool -> (Pos,Cell) -> Html
fromCell possibleMove anyWins (pos,cell) = case (anyWins,cell) of
    (False,Nothing) ->
        button ! name "pos" ! value posVal $
            toHtml $ show possibleMove
        where posVal = toValue $ show pos
    (_, Just madeMove) -> toHtml $ show madeMove
    (True,_) -> text " "

fromBoard :: Move -> Board -> Html
fromBoard move board = form ! method "post" $
    table $ tbody $ forM_ (rowsOf board) $ tr . mapM_ tdFromCell
    where
        tdFromCell = td . fromCell move (anyWinsOn board)

template :: String -> Board -> Html
template msg board = docTypeHtml $ do
    H.head $ do
        title "Tic-tac-toe"
        link ! rel "stylesheet" ! href "/main.css"
        link ! rel "icon" ! href "/favicon.png" ! type_ "image/png"
    body $ do
        h1 $ toHtml $ subsPlayer msg
        fromBoard move board
        form ! action "/restart" $ p $ button "Restart"
    where
        subsPlayer ('{':'}':xs) = "‘" ++ show move ++ "’" ++ xs
        subsPlayer (x:xs) = x : subsPlayer xs
        subsPlayer "" = ""
        move = (if anyWinsOn board then lastMoveOn else nextMoveOn) board
