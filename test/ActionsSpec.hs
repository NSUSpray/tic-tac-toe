{-# LANGUAGE OverloadedStrings #-}

module ActionsSpec (spec) where

import Data.List

import qualified Data.ByteString.Lazy.UTF8 as LazyBytes (toString)
import qualified Data.ByteString.UTF8 as Bytes (toString)
import Test.Hspec
import Test.Hspec.Wai
import Network.Wai.Test (simpleHeaders)

import App (app)
import Board
import Sessions


emptyBoardResponse, nonEmptyBoardResponse :: String
emptyBoardResponse =
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
nonEmptyBoardResponse =
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

xBoard, xxBoard, preFullBoard :: Board
xBoard = fromLists [
    [Nothing,Nothing,Nothing],
    [Nothing,Nothing,Nothing],
    [Just X, Nothing, Nothing]
    ]
xxBoard = fromLists [
    [Nothing, Just O, Just X],
    [Nothing, Just X, Just O],
    [Nothing,Nothing,Nothing]
    ]
preFullBoard = fromLists [
    [Just X, Just O, Just X],
    [Just O, Just O, Just X],
    [Nothing, Just X, Just O]
    ]


match :: String -> (String -> String -> Bool) -> a -> ResponseMatcher
match s p _ = ResponseMatcher 200 [] $ MatchBody $ \_ body ->
    let bodyStr = LazyBytes.toString body in
    if s `p` bodyStr then Nothing
    else Just $
        "Response body\n" ++ show bodyStr ++ "\ndoes nоt satisfy\n" ++ show s

responseBody :: a
responseBody = error
    "It's a dummy for the last argument of 'match' function, not for evaluate."

updateSessionWith :: Board -> WaiSession SessionsVar SessionId
updateSessionWith board = do
    sessionsVar <- getState
    response <- get "/"
    let (Just cookies) = lookup "Set-Cookie" $ simpleHeaders response
        (Just sessionId) = stripPrefix (sessionsCookieName ++ "=") $
            Bytes.toString cookies
    liftIO $ board `writeAtIdTo` sessionsVar $ sessionId


spec :: Spec
spec = withState app $ do

    describe "GET" $

        it "responds with empty board and move of 'X'" $ do
            let getRoot = get "/"
            getRoot `shouldRespondWith` match
                emptyBoardResponse isInfixOf responseBody
            getRoot `shouldRespondWith` match
                "<h1>Move of ‘X’:</h1>" isInfixOf responseBody

    describe "POST" $ do

        let postMove'3'1 = postHtmlForm "/" [("pos","(3,1)")]

        context "when 'X' moves to empty cell" $
            it "responds with updated board and move of 'O'" $ do
                postMove'3'1 `shouldRespondWith` match
                    nonEmptyBoardResponse isInfixOf responseBody
                postMove'3'1 `shouldRespondWith` match
                    "<h1>Move of ‘O’:</h1>" isInfixOf responseBody

        context "when 'O' moves to non-empty cell" $
            it "revokes move and proposes 'O' to choose another cell" $ do
                updateSessionWith xBoard
                postMove'3'1 `shouldRespondWith` match
                    "<h1>This cell is already occupied. ‘O’, please choose \
                        \another:</h1>"
                            isInfixOf responseBody

        context "when 'X' moves and wins" $
            it "congratulates 'X' and finishes the game" $ do
                updateSessionWith xxBoard
                postMove'3'1 `shouldRespondWith` match
                    "<h1>‘X’ wins!</h1>" isInfixOf responseBody
                postMove'3'1 `shouldRespondWith` match
                    "<button name=\"pos\" value=\"("
                        (\a b -> not $ a `isInfixOf` b) responseBody

        context "when 'X' tooks last cell and nobody wins" $
            it "reports a draw and finishes the game" $ do
                updateSessionWith preFullBoard
                postMove'3'1 `shouldRespondWith` match
                    "<h1>You played a draw.</h1>" isInfixOf responseBody
                postMove'3'1 `shouldRespondWith` match
                    "<button name=\"pos\" value=\"("
                        (\a b -> not $ a `isInfixOf` b) responseBody

        context "when 'pos' form param is invalid" $
            it "reports a problem and restarts the game" $ do
                let postMove = postHtmlForm "/" [("pos","{3;1}")]
                postMove `shouldRespondWith` match
                    "<h1>Something went wrong. ‘X’, please try again:</h1>"
                        isInfixOf responseBody
                postMove `shouldRespondWith` match
                    emptyBoardResponse isInfixOf responseBody
