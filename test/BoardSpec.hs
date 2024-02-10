module BoardSpec where

import Control.Exception (evaluate)
import Text.Read (readMaybe)
import Data.Maybe

import Test.Hspec

import Board


sampleBoard, fullBoard :: Board
sampleBoard = fromLists [
    [Just X, Just O, Nothing],
    [Just X, Just X, Nothing],
    [Just O, Nothing, Just X] ]
fullBoard = fromLists $ replicate 3 $ replicate 3 $ Just X


spec :: Spec
spec = do

    describe "Move" $ do
        it "is correct instance of Eq" $
            evaluate (X == X && O == O && X /= O) `shouldReturn` True
        it "is expected instance of Show" $
            return (show X, show O) `shouldReturn` ("X","O")
        it "is expected instance of Read" $
            return [readMaybe "X", readMaybe "O", readMaybe "Oops!"]
                `shouldReturn` [Just X, Just O, Nothing]

    describe "rowsOf" $
        it "gets board's rows" $
            rowsOf (fromLists [[Nothing, Just X], [Just X, Just O]])
                `shouldBe`
                    [ [ ((1,1),Nothing), ((1,2), Just X) ],
                        [ ((2,1), Just X), ((2,2), Just O) ] ]

    describe "emptyBoard" $
        it "makes empty board" $
            emptyBoard `shouldSatisfy` all isNothing

    describe "lastMoveOn" $ do
        context "when last move was X" $
            it "returns X" $
                lastMoveOn sampleBoard `shouldBe` X
        context "when last move was O" $
            it "returns O" $
                lastMoveOn emptyBoard `shouldBe` O

    describe "nextMoveOn" $ do
        context "when last move was X" $
            it "returns O" $
                nextMoveOn sampleBoard `shouldBe` O
        context "when last move was O" $
            it "returns X" $
                nextMoveOn emptyBoard `shouldBe` X

    describe "movedTo" $ do
        context "when target cell is empty" $
            it "returns updated board" $ do
                let maybeNewBoard = X `movedTo` (3,1) $ emptyBoard
                maybeNewBoard `shouldSatisfy` isJust
                getElem 3 1 (fromJust maybeNewBoard) `shouldBe` Just X
        context "when target cell is occupied" $
            it "returns Nothing" $
                (X `movedTo` (3,1) $ sampleBoard) `shouldBe` Nothing
        context "when target cell has invalid indices" $
            it "returns Nothing" $ do
                (X `movedTo` (3,0) $ sampleBoard) `shouldBe` Nothing
                (X `movedTo` (4,1) $ sampleBoard) `shouldBe` Nothing

    describe "isFull" $ do
        context "when where are no empty cells on the board" $
            it "returns True" $
                isFull fullBoard `shouldBe` True
        context "when where is at least one empty cell on the board" $
            it "returns False" $
                isFull sampleBoard `shouldBe` False

    describe "winsOn" $ do
        context "when given player does not win" $
            it "returns False" $
                (O `winsOn` sampleBoard) `shouldBe` False
        context "when given player wins" $
            it "returns True" $
                (X `winsOn` sampleBoard) `shouldBe` True

    describe "anyWinsOn" $ do
        context "when nobody wins" $
            it "returns False" $
                anyWinsOn emptyBoard `shouldBe` False
        context "when anybody wins" $
            it "returns True" $
                anyWinsOn sampleBoard `shouldBe` True
