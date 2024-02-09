{-# LANGUAGE OverloadedStrings #-}

module ActionsSpec where

import Data.List

import Data.ByteString.Lazy.UTF8 (toString)
import Test.Hspec
import Test.Hspec.Wai

import App (app)
import Board


rEmptyBoard, rNonEmptyBoard :: String
rEmptyBoard =
    "<table><tbody><tr>\
    \<td><button name=\"pos\" value=\"(1,1)\">X</button></td>\
    \<td><button name=\"pos\" value=\"(1,2)\">X</button></td>\
    \<td><button name=\"pos\" value=\"(1,3)\">X</button></td>\
    \</tr><tr>\
    \<td><button name=\"pos\" value=\"(2,1)\">X</button></td>\
    \<td><button name=\"pos\" value=\"(2,2)\">X</button></td>\
    \<td><button name=\"pos\" value=\"(2,3)\">X</button></td>\
    \</tr><tr>\
    \<td><button name=\"pos\" value=\"(3,1)\">X</button></td>\
    \<td><button name=\"pos\" value=\"(3,2)\">X</button></td>\
    \<td><button name=\"pos\" value=\"(3,3)\">X</button></td>\
    \</tr></tbody></table>"
rNonEmptyBoard =
    "<table><tbody><tr>\
    \<td><button name=\"pos\" value=\"(1,1)\">O</button></td>\
    \<td><button name=\"pos\" value=\"(1,2)\">O</button></td>\
    \<td><button name=\"pos\" value=\"(1,3)\">O</button></td>\
    \</tr><tr>\
    \<td><button name=\"pos\" value=\"(2,1)\">O</button></td>\
    \<td><button name=\"pos\" value=\"(2,2)\">O</button></td>\
    \<td><button name=\"pos\" value=\"(2,3)\">O</button></td>\
    \</tr><tr>\
    \<td>X</td>\
    \<td><button name=\"pos\" value=\"(3,2)\">O</button></td>\
    \<td><button name=\"pos\" value=\"(3,3)\">O</button></td>\
    \</tr></tbody></table>"

vEmptyBoard, vXBoard, vXXBoard, vPreFullBoard :: String
vEmptyBoard = show $ toLists emptyBoard
vXBoard = show [
    [Nothing,Nothing,Nothing],
    [Nothing,Nothing,Nothing],
    [Just X, Nothing, Nothing]
    ]
vXXBoard = show [
    [Nothing,Nothing,Nothing],
    [Just O, Just X, Nothing],
    [Just X, Just O, Nothing]
    ]
vPreFullBoard = show [
    [Just X, Just O, Just X],
    [Just O, Just O, Just X],
    [Nothing, Just X, Just O]
    ]


match :: String -> (String -> String -> Bool) -> b -> ResponseMatcher
match s p _ = ResponseMatcher 200 [] $ MatchBody $ \_ body ->
    let sBody = toString body in
    if s `p` sBody then Nothing
    else Just $
        "Response body\n" ++ show sBody ++ "\ndoes nоt satisfy\n" ++ show s

respond'sBody :: a
respond'sBody = error
    "It's a dummy for the last argument of 'match' function, not for evaluate."


spec :: Spec
spec = with app $ do

    describe "GET" $

        it "responds with empty board and move of 'X'" $ do
            get "/" `shouldRespondWith` match
                rEmptyBoard isInfixOf respond'sBody
            get "/" `shouldRespondWith` match
                "<h1>Move of ‘X’:</h1>" isInfixOf respond'sBody

    describe "POST" $ do

        context "when 'X' moves to empty cell" $
            it "responds with updated board and move of 'O'" $ do
                let
                    params = [("board", vEmptyBoard), ("pos","(3,1)")]
                    postMove = postHtmlForm "/" params
                postMove `shouldRespondWith` match
                    rNonEmptyBoard isInfixOf respond'sBody
                postMove `shouldRespondWith` match
                    "<h1>Move of ‘O’:</h1>" isInfixOf respond'sBody

        context "when 'O' moves to non-empty cell" $
            it "revokes move and proposes 'O' to choose another cell" $ do
                let
                    params = [("board", vXBoard), ("pos","(3,1)")]
                    postMove = postHtmlForm "/" params
                postMove `shouldRespondWith` match
                    rNonEmptyBoard isInfixOf respond'sBody
                postMove `shouldRespondWith` match
                    "<h1>This cell is already occupied. ‘O’, please choose \
                        \another:</h1>"
                            isInfixOf respond'sBody

        context "when 'X' moves and wins" $
            it "congratulates 'X' and finishes the game" $ do
                let
                    params = [("board", vXXBoard), ("pos","(1,3)")]
                    postMove = postHtmlForm "/" params
                postMove `shouldRespondWith` match
                    "<h1>‘X’ wins!</h1>" isInfixOf respond'sBody
                postMove `shouldRespondWith` match
                    "<button name=\"pos\" value=\"("
                        (\a b -> not $ a `isInfixOf` b) respond'sBody

        context "when 'X' tooks last cell and nobody wins" $
            it "reports a draw and finishes the game" $ do
                let
                    params = [("board", vPreFullBoard), ("pos","(3,1)")]
                    postMove = postHtmlForm "/" params
                postMove `shouldRespondWith` match
                    "<h1>You played a draw.</h1>" isInfixOf respond'sBody
                postMove `shouldRespondWith` match
                    "<button name=\"pos\" value=\"("
                        (\a b -> not $ a `isInfixOf` b) respond'sBody

        context "when any form param is invalid" $
            it "reports a problem and restarts the game" $ do
                let
                    params1 = [("board", tail vEmptyBoard), ("pos","(3,1)")]
                    params2 = [("board", vEmptyBoard), ("pos","{3;1}")]
                    h1 = "<h1>Something went wrong. You’ll have\
                        \ to start over :(</h1>"
                mapM_ (\params -> do
                    let postMove = postHtmlForm "/" params
                    postMove `shouldRespondWith` match
                        h1 isInfixOf respond'sBody
                    postMove `shouldRespondWith` match
                        rEmptyBoard isInfixOf respond'sBody
                    ) [params1,params2]
