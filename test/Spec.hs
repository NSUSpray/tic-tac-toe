module Main where

import Test.Hspec

import qualified ActionsSpec
import qualified BoardSpec
import qualified HtmlSpec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Actions" ActionsSpec.spec
    describe "Board" BoardSpec.spec
    describe "Html" HtmlSpec.spec
