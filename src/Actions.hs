{-# LANGUAGE OverloadedStrings #-}

module Actions (processMove,requestMove,restart) where

import Text.Read (readMaybe)

import Data.ByteString.UTF8 (fromString)
import qualified Data.Map as Map
import Data.Text (pack,unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S
import Web.Scotty.Cookie

import Board
import qualified Html
import qualified Sessions


response :: String -> Board -> S.ActionM ()
response msg = S.html . renderHtml . Html.template msg

movingResponse :: Board -> S.ActionM ()
movingResponse = response "Move of {}:"


startNewSession :: Sessions.Var -> S.ActionM Sessions.Id
startNewSession sessionsVar = do
    newSessionId <- S.liftIO $ Sessions.createNew sessionsVar
    setCookie $ defaultSetCookie {
        setCookieName = fromString Sessions.cookieName,
        setCookieValue = fromString newSessionId
        }
    return newSessionId

readSessionId :: Sessions.Var -> S.ActionM Sessions.Id
readSessionId sessionsVar = do
    maybeSessionId <- getCookie $ pack Sessions.cookieName
    case maybeSessionId of
        Nothing ->
            startNewSession sessionsVar
        Just sessionIdTxt -> do
            sessions <- S.liftIO $ Sessions.read sessionsVar
            if sessionId `Map.member` sessions then
                return sessionId
            else
                startNewSession sessionsVar
            where sessionId = unpack sessionIdTxt


readBoard :: Sessions.Var -> S.ActionM Board
readBoard sessionsVar = do
    sessionId <- readSessionId sessionsVar
    sessions <- S.liftIO $ Sessions.read sessionsVar
    let (Just board) = Map.lookup sessionId sessions
    return board

writeBoard :: Board -> Sessions.Var -> S.ActionM ()
writeBoard board sessionsVar = do
    sessionId <- readSessionId sessionsVar
    S.liftIO $ Sessions.writeBoard sessionsVar sessionId board


requestMove :: Sessions.Var -> S.ActionM ()
requestMove sessionsVar = do
    board <- readBoard sessionsVar
    movingResponse board

processMove :: Sessions.Var -> S.ActionM ()
processMove sessionsVar = do
    posStr <- S.formParam "pos"
    currentBoard <- readBoard sessionsVar
    let orFinish maybeVal msg = case maybeVal of
            Nothing -> do { response msg currentBoard; S.finish }
            Just res -> return res
    newPos <- readMaybe posStr
        `orFinish` "Something went wrong. {}, please try again:"
    let currentPlayer = nextMoveOn currentBoard
    updatedBoard <- (currentPlayer `movedTo` newPos $ currentBoard)
        `orFinish` "This cell is already occupied. {}, please choose another:"
    writeBoard updatedBoard sessionsVar
    ($ updatedBoard) $
        if currentPlayer `winsOn` updatedBoard then
            response "{} wins!"
        else if ($ updatedBoard) isFull then
            response "You played a draw."
        else
            movingResponse

restart :: Sessions.Var -> S.ActionM ()
restart sessionsVar = do
    writeBoard emptyBoard sessionsVar
    S.redirect "/"
