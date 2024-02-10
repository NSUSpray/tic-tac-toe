{-# LANGUAGE OverloadedStrings #-}

module App (app,run) where

import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import qualified Web.Scotty as S

import Board (Move(X),emptyBoard)
import Actions


routes :: S.ScottyM ()
routes = do
    S.middleware $ staticPolicy (noDots >-> addBase "static")
    S.get "/" $ movingResponse emptyBoard
    S.post "/" processMove

app :: IO Application
app = S.scottyApp routes

run :: IO ()
run = let port = 3000 in S.scotty port $ do
    S.middleware logStdoutDev
    routes
