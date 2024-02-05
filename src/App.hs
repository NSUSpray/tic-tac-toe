{-# LANGUAGE OverloadedStrings #-}

module App (app,runApp) where

import qualified Web.Scotty as S
import Network.Wai (Application)
import Network.Wai.Middleware.Static

import Board (Move(X),emptyBoard)
import Actions


routes :: S.ScottyM ()
routes = do
    S.middleware $ staticPolicy (noDots >-> addBase "static")
    S.get "/" $ movingResponse X emptyBoard
    S.post "/" processRequest

app :: IO Application
app = S.scottyApp routes

runApp :: IO ()
runApp = S.scotty port routes
    where port = 3000
