module BoardToHtmlSpec where

import Data.List

import Test.Hspec
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Board
import BoardToHtml


fullNotWinBoard, winBoard :: Board
fullNotWinBoard = fromLists [
    [Just X, Just O, Just X],
    [Just X, Just O, Just O],
    [Just O, Just X, Just X]
    ]
winBoard = fromLists [
    [Just X, Nothing, Just O],
    [Just X, Just O, Just O],
    [Just O, Just X, Nothing]
    ]

showBoard :: Board -> String
showBoard = show . renderHtml . boardToHtml X

buttonSubstring :: String
buttonSubstring = "<button name=\\\"pos\\\" value=\\\"("


spec = do
    describe "boardToHtml" $ do
        it "contains form with table inside" $ do
            showBoard emptyBoard `shouldSatisfy`
                ("\"<form method=\\\"post\\\">\
                    \<input type=\\\"hidden\\\" name=\\\"move\\\" value=\\\"X\\\">\
                    \<input type=\\\"hidden\\\" name=\\\"board\\\" value=\\\"\
                        \[[Nothing,Nothing,Nothing],\
                        \[Nothing,Nothing,Nothing],\
                        \[Nothing,Nothing,Nothing]]\
                    \\\\"><table><tbody><tr><td>" `isPrefixOf`)
            showBoard emptyBoard `shouldSatisfy`
                ("</td></tr></tbody></table></form>\"" `isSuffixOf`)
        context "when nobody wins and where is at least one empty cell" $
            it "contains button for possible move" $
                showBoard emptyBoard `shouldSatisfy`
                    (buttonSubstring `isInfixOf`)
        context "when where are no empty cells on the board" $
            it "contains only contents of cells" $
                showBoard fullNotWinBoard `shouldNotSatisfy`
                    (buttonSubstring `isInfixOf`)
        context "when anybody wins" $
            it "contains only contents of cells" $
                showBoard winBoard `shouldNotSatisfy`
                    (buttonSubstring `isInfixOf`)
