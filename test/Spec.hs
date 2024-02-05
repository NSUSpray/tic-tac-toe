{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.Hspec.Wai

import App (app)


main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $ do
    describe "GET /" $ do
        it "reponds with 200" $ do
            get "/" `shouldRespondWith` 200

        it "reponds with 'hello'" $ do
            get "/" `shouldRespondWith` "hello"
