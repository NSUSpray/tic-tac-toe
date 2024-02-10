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
spec = describe "fromBoard" $ do
    let showBoard = unpack . renderHtml . Html.fromBoard X
    it "contains form with table inside" $ do
        showBoard emptyBoard `shouldStartWith`
            "<form method=\"post\">\
                \<input type=\"hidden\" name=\"board\" value=\"\
                    \[[Nothing,Nothing,Nothing],\
                    \[Nothing,Nothing,Nothing],\
                    \[Nothing,Nothing,Nothing]]\
                \\"><table><tbody><tr><td>"
        showBoard emptyBoard `shouldEndWith`
            "</td></tr></tbody></table></form>"
    context "when nobody wins and where is at least one empty cell" $
        it "contains button for possible move" $
            showBoard emptyBoard `shouldContain` buttonSubstring
    context "when where are no empty cells on the board" $
        it "contains only contents of cells" $
            showBoard fullNotWinBoard `shouldNotContain` buttonSubstring
    context "when anybody wins" $
        it "contains only contents of cells" $
            showBoard winBoard `shouldNotContain` buttonSubstring
