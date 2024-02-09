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
    input
        ! type_ "hidden"
        ! name "board"
        ! value (toValue $ show $ toLists board)
    table $ tbody $ forM_ (rowsOf board) $ tr . mapM_ tdFromCell
    where
        tdFromCell = td . cellToHtml move (anyWinsOn board)
