module BoardSpec where

import Text.Read (readMaybe)

import Test.Hspec
import Data.Maybe (isJust,fromJust)

import Board


sampleBoard, fullBoard :: Board

sampleBoard = fromLists [
    [Just X, Just O, Nothing],
    [Just X, Just X, Nothing],
    [Just O, Nothing, Just X] ]

fullBoard = fromLists $ replicate 3 $ replicate 3 $ Just X


spec = do

    describe "Move" $ do
        it "is instance of Eq" $
            X==X && O==O && X/=O `shouldBe` True
        it "is instance of Show" $
            (show X, show O) `shouldBe` ("X","O")
        it "is instance of Read" $
            [readMaybe "X", readMaybe "O", readMaybe "Oops!"]
                `shouldBe` [Just X, Just O, Nothing]

    describe "rowsOf" $
        it "gets board's rows" $
            rowsOf (fromLists [[Nothing, Just X], [Just X, Just O]])
                `shouldBe`
                    [ [ ((1,1),Nothing), ((1,2), Just X) ],
                        [ ((2,1), Just X), ((2,2), Just O) ] ]

    describe "emptyBoard" $
        it "makes empty board of size 3x3" $
            emptyBoard `shouldBe`
                fromLists (replicate 3 $ replicate 3 Nothing)

    describe "moveAfter" $
        it "returns move following given" $ do
            moveAfter X `shouldBe` O
            moveAfter O `shouldBe` X

    describe "movedTo" $ do
        context "when target cell is empty" $
            it "returns updated board" $ do
                let maybeNewBoard = X `movedTo` (3,1) $ emptyBoard
                maybeNewBoard `shouldSatisfy` isJust
                getElem 3 1 (fromJust maybeNewBoard) `shouldBe` Just X
        context "when target cell is occupied" $
            it "returns Nothing" $
                (X `movedTo` (3,1) $ sampleBoard) `shouldBe` Nothing

    describe "isFull" $ do
        context "when where are no empty cells on the board" $
            it "returns True" $
                fullBoard `shouldSatisfy` isFull
        context "when where is at least one empty cell on the board" $
            it "returns False" $
                sampleBoard `shouldNotSatisfy` isFull

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
                emptyBoard `shouldNotSatisfy` anyWinsOn
        context "when anybody wins" $
            it "returns True" $
                sampleBoard `shouldSatisfy` anyWinsOn
