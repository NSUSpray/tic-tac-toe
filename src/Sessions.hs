module Sessions where

import Control.Concurrent.STM.TVar
import Control.Monad.STM (atomically)
import Data.ByteString.UTF8 (toString)
import qualified Data.Map as Map
import Network.Wai.Session (genSessionId)

import Board


type Id = String
type Sessions = Map.Map Id Board
type Var = TVar Sessions


cookieName :: String
cookieName = "session_id"

init :: IO Var
init = newTVarIO emptySessions
    where emptySessions = Map.empty

read :: Var -> IO Sessions
read = readTVarIO

createNew :: Var -> IO Id
createNew sessionsVar = do
    newSessionIdBStr <- genSessionId
    let newSessionId = toString newSessionIdBStr
    writeBoard sessionsVar newSessionId emptyBoard
    return newSessionId

writeBoard :: Var -> Id -> Board -> IO ()
writeBoard sessionsVar sessionId =
    atomically . modifyTVar sessionsVar . Map.insert sessionId
