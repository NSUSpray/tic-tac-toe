{-# LANGUAGE OverloadedStrings #-}

module ActionsSpec where

import Data.List

import Test.Hspec
import Test.Hspec.Wai

import App (app)
import Board


rEmptyBoard, rNonEmptyBoard :: String
rEmptyBoard =
    "<table><tbody><tr>\
    \<td><button name=\\\"pos\\\" value=\\\"(1,1)\\\">X</button></td>\
    \<td><button name=\\\"pos\\\" value=\\\"(1,2)\\\">X</button></td>\
    \<td><button name=\\\"pos\\\" value=\\\"(1,3)\\\">X</button></td>\
    \</tr><tr>\
    \<td><button name=\\\"pos\\\" value=\\\"(2,1)\\\">X</button></td>\
    \<td><button name=\\\"pos\\\" value=\\\"(2,2)\\\">X</button></td>\
    \<td><button name=\\\"pos\\\" value=\\\"(2,3)\\\">X</button></td>\
    \</tr><tr>\
    \<td><button name=\\\"pos\\\" value=\\\"(3,1)\\\">X</button></td>\
    \<td><button name=\\\"pos\\\" value=\\\"(3,2)\\\">X</button></td>\
    \<td><button name=\\\"pos\\\" value=\\\"(3,3)\\\">X</button></td>\
    \</tr></tbody>"
rNonEmptyBoard =
    "<table><tbody><tr>\
    \<td><button name=\\\"pos\\\" value=\\\"(1,1)\\\">O</button></td>\
    \<td><button name=\\\"pos\\\" value=\\\"(1,2)\\\">O</button></td>\
    \<td><button name=\\\"pos\\\" value=\\\"(1,3)\\\">O</button></td>\
    \</tr><tr>\
    \<td><button name=\\\"pos\\\" value=\\\"(2,1)\\\">O</button></td>\
    \<td><button name=\\\"pos\\\" value=\\\"(2,2)\\\">O</button></td>\
    \<td><button name=\\\"pos\\\" value=\\\"(2,3)\\\">O</button></td>\
    \</tr><tr>\
    \<td>X</td>\
    \<td><button name=\\\"pos\\\" value=\\\"(3,2)\\\">O</button></td>\
    \<td><button name=\\\"pos\\\" value=\\\"(3,3)\\\">O</button></td>\
    \</tr></tbody>"

vEmptyBoard, vXBoard, vXXBoard, vPreFullBoard :: String
vEmptyBoard = show $ toLists emptyBoard
vXBoard = show [
    [Nothing,Nothing,Nothing],
    [Nothing,Nothing,Nothing],
    [Just X, Nothing, Nothing]
    ]
vXXBoard = show [
    [Nothing,Nothing,Nothing],
    [Nothing, Just X, Nothing],
    [Just X, Nothing, Nothing]
    ]
vPreFullBoard = show [
    [Just X, Just O, Just X],
    [Just X, Just O, Just O],
    [Nothing, Just X, Just X]
    ]


match :: Show a => a -> (a -> String -> Bool) -> b -> ResponseMatcher
match s p _ = ResponseMatcher 200 [] $ MatchBody $ \_ body ->
    if s `p` show body then Nothing
        else Just $
            "Response body\n" ++ show body ++ "\ndoes nоt satisfy\n" ++ show s

respond'sBody :: a
respond'sBody = undefined


spec = with app $ do

    describe "GET" $

        it "responds with empty board and move of 'X'" $ do
            get "/" `shouldRespondWith`
                match rEmptyBoard isInfixOf respond'sBody
            get "/" `shouldRespondWith`
                match "<h1>Move of \\226\\128\\152X\\226\\128\\153:</h1>"
                    isInfixOf respond'sBody

    describe "POST" $ do

        context "when 'X' moves to empty cell" $
            it "responds with updated board and move of 'O'" $ do
                let
                    params =
                        [("move","X"), ("board", vEmptyBoard), ("pos","(3,1)")]
                    postMove = postHtmlForm "/" params
                postMove `shouldRespondWith`
                    match rNonEmptyBoard isInfixOf respond'sBody
                postMove `shouldRespondWith`
                    match "<h1>Move of \\226\\128\\152O\\226\\128\\153:</h1>"
                        isInfixOf respond'sBody

        context "when 'O' moves to non-empty cell" $
            it "revokes move and proposes 'O' to choose another cell" $ do
                let
                    params =
                        [("move","O"), ("board", vXBoard), ("pos","(3,1)")]
                    postMove = postHtmlForm "/" params
                postMove `shouldRespondWith`
                    match rNonEmptyBoard isInfixOf respond'sBody
                postMove `shouldRespondWith` match
                    "<h1>This cell is already occupied. \\226\\128\\152O\\226\
                        \\\128\\153, please choose another:</h1>"
                            isInfixOf respond'sBody

        context "when 'X' moves and wins" $
            it "congratulates 'X' and finishes the game" $ do
                let
                    params =
                        [("move","X"), ("board", vXXBoard), ("pos","(1,3)")]
                    postMove = postHtmlForm "/" params
                postMove `shouldRespondWith` match
                    "<h1>\\226\\128\\152X\\226\\128\\153 wins!</h1>"
                            isInfixOf respond'sBody
                postMove `shouldRespondWith` match
                    "<button name=\\\"pos\\\" value=\\\"("
                        (\a b -> not $ a `isInfixOf` b) respond'sBody

        context "when 'O' tooks last cell and nobody wins" $
            it "reports a draw and finishes the game" $ do
                let
                    params =
                        [("move","O"), ("board", vPreFullBoard), ("pos","(3,1)")]
                    postMove = postHtmlForm "/" params
                postMove `shouldRespondWith` match
                    "<h1>You played a draw.</h1>" isInfixOf respond'sBody
                postMove `shouldRespondWith` match
                    "<button name=\\\"pos\\\" value=\\\"("
                        (\a b -> not $ a `isInfixOf` b) respond'sBody

        context "when any form param is invalid" $
            it "reports a problem and restarts the game" $ do
                let
                    ps1 = [("move","Oops!"), ("board", vEmptyBoard), ("pos","(3,1)")]
                    ps2 = [("move","X"), ("board", tail vEmptyBoard), ("pos","(3,1)")]
                    ps3 = [("move","X"), ("board", vEmptyBoard), ("pos","{3;1}")]
                    h1 = "<h1>Something went wrong. You\\226\\128\\153ll have\
                        \ to start over :(</h1>"
                mapM_ (\params -> do
                    let postMove = postHtmlForm "/" params
                    postMove `shouldRespondWith` match
                        h1 isInfixOf respond'sBody
                    postMove `shouldRespondWith` match
                        rEmptyBoard isInfixOf respond'sBody
                    ) [ps1,ps2,ps3]
