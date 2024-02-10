{-# LANGUAGE OverloadedStrings #-}

module Html (fromBoard) where

import Control.Monad (forM_)

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (form)

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
fromBoard move board = form ! method "post" $ do
    input
        ! type_ "hidden"
        ! name "board"
        ! value (toValue $ show $ toLists board)
    table $ tbody $ forM_ (rowsOf board) $ tr . mapM_ tdFromCell
    where
        tdFromCell = td . fromCell move (anyWinsOn board)
