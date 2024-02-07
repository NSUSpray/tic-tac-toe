module Main where

import Test.Hspec

import qualified ActionsSpec
import qualified BoardSpec
import qualified BoardToHtmlSpec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Actions" ActionsSpec.spec
    describe "Board" BoardSpec.spec
    describe "BoardToHtml" BoardToHtmlSpec.spec
