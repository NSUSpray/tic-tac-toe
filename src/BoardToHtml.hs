{-# LANGUAGE OverloadedStrings #-}

module BoardToHtml (boardToHtml) where

import Control.Monad (forM_)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (form)

import Board


cellToHtml :: Move -> Bool -> (Pos,Cell) -> Html
cellToHtml possibleMove anyWins (pos,cell) = case (anyWins,cell) of
    (False,Nothing) ->
        button ! name "pos" ! value vPos $
            toHtml $ show possibleMove
        where vPos = toValue $ show pos
    (_, Just madeMove) -> toHtml $ show madeMove
    (True,_) -> text " "

boardToHtml :: Move -> Board -> Html
boardToHtml move board = form ! method "post" $ do
    move `storeAs` "move"
    toLists board `storeAs` "board"
    table $ tbody $ forM_ (rowsOf board) $ tr . mapM_ tdFromCell
    where
        tValue `storeAs` vName = input
            ! type_ "hidden"
            ! name vName
            ! value (toValue $ show tValue)
        tdFromCell = td . cellToHtml move (anyWinsOn board)
