{-# LANGUAGE OverloadedStrings #-}

module BoardToHtml (boardToHtml) where

import Control.Monad (forM_)

import Data.Matrix (toLists)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (form)

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
