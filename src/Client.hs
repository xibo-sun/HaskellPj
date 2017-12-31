module Main where

import Network (PortID(PortNumber), connectTo, withSocketsDo)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import System.Environment (getArgs)
import System.Console.GetOpt  
import System.Random
import Control.Monad (when)
import FRP.Yampa ((>>>), arr)

import Common
import Net
import RunGame
import Object


options :: [OptDescr (GameConfig -> IO GameConfig)]
options = [
    Option ['n'] ["name"]   (ReqArg (\arg gc -> return gc{gcPlayerName=arg}) "FILE")  "input file to read",
    Option ['t'] ["tracker"] (ReqArg (\arg gc -> return gc{gcTracker=arg}) "FILE")    "address of server tracker"
  ]




-- Use host-name only leaving it out fails.
-- Server hostname is kept track by the remote serverTracker
main :: IO ()
main = withSocketsDo $ do -- withSocketsDo is only needed for Windows platform, harmless on others
    args <- getArgs
    let ( actions, _, _) = getOpt Permute options args
    config <- foldl (>>=) (return defaultConfigs) actions

    let playerName = gcPlayerName config
    when (playerName == "uninitialized" ) $ error "Wrong syntax.  Syntax: ./Client -n <player-name>"

    -- Ask remote server tracker which server is on

    -- print $ gcTracker config
    -- r <- simpleHTTP (getRequest $ gcTracker config)
    -- serverHost <- getResponseBody r
    -- print serverHost
    -- if serverHost == "NOSERVER" then error "No server is open." else return serverHost

    -- print ("Connecting player " ++ playerName ++ " to " ++ serverHost)
    let serverHost = gcTracker config
    print serverHost
    -- Ask server to connect
    handle <- connectTo serverHost (PortNumber 4444)

    -- Tell server that this player is joining
    sendCSMsg handle $ (-1, CSMsgJoin playerName)

    -- Prepare some OpenGL intialization and windows management
    gameInit

    let g = mkStdGen 2
    let initialObjs = [serverObject g playerName, terrain0]

    -- TODO: Explain runGame
    runGame config (Just handle) (game initialObjs >>> (arr (\(ooss,msgs) -> (renderObsObjStates ooss, sendNetworkMsgs handle msgs))))
  where renderObsObjStates = foldl (\io oos -> io >> renderObsObjState oos) (return ())
        sendNetworkMsgs h = foldl (\io msg -> io >> sendCSMsg h msg) (return ())

