module HtmlSpec where

import Data.Text.Lazy (unpack)
import Test.Hspec
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Board
import qualified Html


fullNotWinBoard, winBoard :: Board
fullNotWinBoard = fromLists [
    [Just X, Just O, Just X],
    [Just X, Just O, Just O],
    [Just O, Just X, Just X]
    ]
winBoard = fromLists [
    [Just X, Nothing, Just O],
    [Just X, Just O, Nothing],
    [Just O, Just X, Nothing]
    ]

buttonSubstring :: String
buttonSubstring = "<button name=\"pos\" value=\"("


spec :: Spec
spec = describe "template" $ do
    let showTemplate = unpack . renderHtml . Html.template "Move of {}:"
    it "contains html with form and table inside" $ do
        showTemplate emptyBoard `shouldStartWith`
            "<!DOCTYPE HTML>\n<html><head><title>Tic-tac-toe</title>\
                \<link rel=\"stylesheet\" href=\"/main.css\">\
                \<link rel=\"icon\" href=\"/favicon.png\" type=\"image/png\">\
                \</head><body><h1>Move of ‘X’:</h1>\
                \<form method=\"post\"><table><tbody><tr><td>"
        showTemplate emptyBoard `shouldEndWith`
            "</td></tr></tbody></table></form>\
            \<form action=\"/restart\"><p><button>Restart</button></p></form>\
            \</body></html>"
    context "when nobody wins and where is at least one empty cell" $
        it "contains button for possible move" $
            showTemplate emptyBoard `shouldContain` buttonSubstring
    context "when where are no empty cells on the board" $
        it "contains only contents of cells" $
            showTemplate fullNotWinBoard `shouldNotContain` buttonSubstring
    context "when anybody wins" $
        it "contains only contents of cells" $
            showTemplate winBoard `shouldNotContain` buttonSubstring
