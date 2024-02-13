module Sessions where

import Control.Concurrent.STM.TVar
import Control.Monad.STM (atomically)
import Data.ByteString.UTF8 (fromString,toString)
import qualified Data.Map as Map
import Network.Wai.Session (genSessionId)
import Web.Scotty.Cookie

import Board (Board,emptyBoard)


type SessionId = String
type Sessions = Map.Map SessionId Board
type SessionsVar = TVar Sessions


sessionsCookieName :: String
sessionsCookieName = "session_id"

cookie :: String -> SetCookie
cookie value =  defaultSetCookie {
    setCookieName = fromString sessionsCookieName,
    setCookieValue = fromString value
    }

lookupBoard :: SessionId -> Sessions -> Maybe Board
lookupBoard = Map.lookup

isKeyOf :: SessionId -> Sessions -> Bool
isKeyOf = Map.member

initSessions :: IO SessionsVar
initSessions = newTVarIO emptySessions
    where emptySessions = Map.empty

readSessions :: SessionsVar -> IO Sessions
readSessions = readTVarIO

writeAtIdTo :: Board -> SessionsVar -> SessionId -> IO SessionId
(board `writeAtIdTo` sessionsVar) sessionId = do
    atomically $ modifyTVar sessionsVar $ Map.insert sessionId board
    return sessionId

createNewSession :: SessionsVar -> IO SessionId
createNewSession sessionsVar =
    genSessionId >>= (emptyBoard `writeAtIdTo` sessionsVar) . toString
