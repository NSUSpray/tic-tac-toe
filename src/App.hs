{-# LANGUAGE OverloadedStrings #-}

module App (app,run) where

import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import qualified Web.Scotty as S

import Board (Move(X),emptyBoard)
import Actions
import Sessions (SessionsVar,initSessions)


routes :: SessionsVar -> S.ScottyM ()
routes sessionsVar = do
    S.middleware $ staticPolicy (noDots >-> addBase "static")
    S.get "/" $ requestMove sessionsVar
    S.post "/" $ processMove sessionsVar
    S.get "/restart" $ restart sessionsVar

app :: IO (SessionsVar,Application)
app = do
    sessionsVar <- initSessions
    application <- S.scottyApp $ routes sessionsVar
    return (sessionsVar,application)

run :: IO ()
run = do
    sessionsVar <- initSessions
    let port = 3000 in S.scotty port $
        S.middleware logStdoutDev >> routes sessionsVar
