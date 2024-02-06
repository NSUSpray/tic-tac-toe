module Main where

import Test.Hspec

import qualified BoardSpec


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Board" BoardSpec.spec
