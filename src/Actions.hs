{-# LANGUAGE OverloadedStrings #-}

module Actions (processMove,requestMove,restart) where

import Control.Monad ((>=>))
import Text.Read (readMaybe)

import Data.Text (pack,unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Web.Scotty as S
import Web.Scotty.Cookie (getCookie,setCookie)

import Board
import qualified Html
import Sessions


response :: String -> Board -> S.ActionM ()
response msg = S.html . renderHtml . Html.template msg

movingResponse :: Board -> S.ActionM ()
movingResponse = response "Move of {}:"


startNewSession :: SessionsVar -> S.ActionM SessionId
startNewSession sessionsVar = do
    newSessionId <- S.liftIO $ createNewSession sessionsVar
    setCookie $ cookie newSessionId
    return newSessionId

readSessionId :: SessionsVar -> S.ActionM SessionId
readSessionId sessionsVar = do
    maybeSessionId <- getCookie $ pack sessionsCookieName
    case maybeSessionId of
        Nothing ->
            startNewSession sessionsVar
        Just sessionIdTxt -> do
            sessions <- S.liftIO $ readSessions sessionsVar
            if sessionId `isKeyOf` sessions then
                return sessionId
            else
                startNewSession sessionsVar
            where sessionId = unpack sessionIdTxt


readBoardFrom :: SessionsVar -> S.ActionM Board
readBoardFrom sessionsVar = do
    sessionId <- readSessionId sessionsVar
    sessions <- S.liftIO $ readSessions sessionsVar
    let (Just board) = lookupBoard sessionId sessions
    return board

writeTo :: Board -> SessionsVar -> S.ActionM SessionId
board `writeTo` sessionsVar =
    readSessionId sessionsVar >>= S.liftIO . (board `writeAtIdTo` sessionsVar)


requestMove :: SessionsVar -> S.ActionM ()
requestMove = readBoardFrom >=> movingResponse

processMove :: SessionsVar -> S.ActionM ()
processMove sessionsVar = do
    posStr <- S.formParam "pos"
    currentBoard <- readBoardFrom sessionsVar
    let maybeValue `orFinish` msg = case maybeValue of
            Nothing -> response msg currentBoard >> S.finish
            Just res -> return res
    newPos <- readMaybe posStr
        `orFinish` "Something went wrong. {}, please try again:"
    let currentPlayer = nextMoveOn currentBoard
    updatedBoard <- (currentPlayer `movedTo` newPos $ currentBoard)
        `orFinish` "This cell is already occupied. {}, please choose another:"
    updatedBoard `writeTo` sessionsVar
    ($ updatedBoard) $
        if currentPlayer `winsOn` updatedBoard then
            response "{} wins!"
        else if ($ updatedBoard) isFull then
            response "You played a draw."
        else
            movingResponse

restart :: SessionsVar -> S.ActionM ()
restart sessionsVar = emptyBoard `writeTo` sessionsVar >> S.redirect "/"
