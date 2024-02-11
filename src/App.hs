{-# LANGUAGE OverloadedStrings #-}

module App (app,run) where

import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import qualified Web.Scotty as S

import Board (Move(X),emptyBoard)
import Actions
import qualified Sessions (Var,init)


routes :: Sessions.Var -> S.ScottyM ()
routes sessionsVar = do
    S.middleware $ staticPolicy (noDots >-> addBase "static")
    S.get "/" $ requestMove sessionsVar
    S.post "/" $ processMove sessionsVar
    S.get "/restart" $ restart sessionsVar

app :: IO (Sessions.Var,Application)
app = do
    sessionsVar <- Sessions.init
    application <- S.scottyApp $ routes sessionsVar
    return (sessionsVar,application)

run :: IO ()
run = do
    sessionsVar <- Sessions.init
    let port = 3000 in S.scotty port $ do
        S.middleware logStdoutDev
        routes sessionsVar
