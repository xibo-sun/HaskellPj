{-# LANGUAGE Arrows #-}
module Main where

import FRP.Yampa
import System.IO
import Control.Concurrent
import Control.Monad
import Control.Exception hiding (handle)
import Data.Time.Clock
import Data.List
import Data.Maybe
import System.IO.Error
import Collection
import Network
import Network.HTTP (getRequest, simpleHTTP, getResponseBody)

import Common
import CallBackFunc
import CollisionTest
import Net
import Vec3d

data ServerState = ServerState {handles :: ![(Int, Handle)],
                                nextID :: !Int,
                                lastExitID :: !Int,
                                allPlayers :: ![Player],
                                allBullets :: !(IL Bullet)}--TODO: why IL on Lasers and why not on Players
    deriving (Show, Eq)

data ServerInput = ServerInput {msg :: !CSMsg,
                                handle :: !(Maybe Handle)}
    deriving (Show, Eq)

emptyServerState :: ServerState
emptyServerState = ServerState{handles=[], nextID=0, lastExitID=(-1), allPlayers=[],allBullets=emptyIL}

dummyServerInput :: ServerInput
dummyServerInput = ServerInput {msg = dummyCSMsg, handle = Nothing}

fetchCSMsg :: ReactChan ServerInput -> Handle -> IO ()
fetchCSMsg rch h = do
    ln <- hGetLine h
    print "CS message: "
    printFlush ln
    addToReact rch (\si -> si {msg = destringify ln, handle = Just h})

main :: IO ()
main = do
    runServer (PortNumber 4444) server

runServer :: PortID -> SF ServerInput (IO()) -> IO ()
runServer port sf = withSocketsDo $ do
          -- 监听端口
          sock <- listenOn port

          -- 初始化一个反应处理器，接受输入信号，并进行一定操作
          {- type ReactHandle a b = IORef (ReactState a b)

          reactInit :: IO a -> (ReactHandle a b -> Bool -> b -> IO Bool) -> SF a b -> IO (ReactHandle a b)
          -- IO a ： 输入信号
          -- (ReactHandle a b -> Bool -> b -> IO Bool) ： 激励函数，将输入送入信号函数
          -- SF a b ：信号函数 ，a 代表输入信号，b 代表输出信号
          -- 在这里, b 为一系列 IO() 操作
          -}
          rh <- reactInit (return dummyServerInput) (\_ _ sendmsgs -> sendmsgs >> return False) sf
          rch <- newChan

          -- one thread listens for new players joining
          -- forkIO :: IO () -> IO ThreadId
          forkIO $ acceptClient rch sock

          -- write to chan once in a while to keep the server hard at work, so that server is updated
          forkIO $ do
                let loop = do
                      addToReact rch id
                      threadDelay 1000    -- Microseconds
                      loop
                loop

          startTime <- getCurrentTime
          -- main thread process.
          -- this readChan/unGetChan makes it more efficent (server idle instead of looping)
          let loop lTime lastA = do
                newA <- getReactInput rch lastA
                curTime <- getCurrentTime
                react rh (fromRational . toRational $ diffUTCTime curTime lTime, Just newA)
                loop curTime newA
          loop startTime dummyServerInput
          
      where acceptClient rch sock = do
                -- accept :: Socket-> IO (Handle, HostName, PortNumber)
                (hand,_,_) <- accept sock
                -- hIsOpen :: Handle -> IO Bool
                -- 检测套接字是否打开
                open <- hIsOpen hand

                printFlush ("Accepting, verify opened: " ++ show open)
                forkIO $ do
                    let loop = do
                           succ <- hWaitForInput hand (-1)
                           when succ $ fetchCSMsg rch hand
                           loop
                    -- When player quits, handle becomes invalid (closed by main thread), thus exception thrown
                    catch loop (\(SomeException e) -> print "Player quit.")
                acceptClient rch sock

server :: SF ServerInput (IO())
server = proc si -> do
    -- loopPre :: c -> SF (a, c) (b, c) -> SF a b
    -- Loop with an initial value for the signal being fed back.
    msgs <- loopPre (emptyServerState, emptyServerState) objSF -< si
    returnA -< msgs

objSF :: SF (ServerInput, (ServerState, ServerState)) (IO(), (ServerState, ServerState))
objSF = proc (si, (sprev, s0)) -> do
    -- edgeBy :: (a -> a -> Maybe b) -> a -> SF a (Event b)
    -- 跳变检测器（输入为 a 的状态）
    -- (a -> a -> Maybe b) 为跳变检测函数
    -- a 初始状态
    inputChange <- edgeBy (\old new -> if old/=new then Just new else Nothing) dummyServerInput -< si


    let s1 = updateServerState (s0,inputChange)

    lps <- moveObjs allBullets bulletPos bulletVel -< s1           -- calc current lasers' pos
    let s2 = s1 {allBullets = zipWithIL (\l p -> l {bulletPos = p}) (const Nothing) (const Nothing) (allBullets s1) lps}
        hits = checkHits (sprev, s2)
        scMsgs = toMessages (s2, inputChange, hits)

    returnA -< (sendAll s2 scMsgs, (s2, s1))

updateServerState :: (ServerState, Event ServerInput) -> ServerState
updateServerState (s, Event ServerInput{msg=(_, CSMsgPlayer p)})               = s{allPlayers = map (\x->if playerID x == playerID p then p else x) $ allPlayers s}
updateServerState (s, Event ServerInput{msg=(_, CSMsgUpdate p)})               = s{allPlayers = map (\x->if playerID x == playerID p then p else x) $ allPlayers s}
updateServerState (s, Event ServerInput{msg=(_, CSMsgBullet b)})               = s{allBullets  = insertIL b $ allBullets s}
updateServerState (s, Event ServerInput{msg=(_, CSMsgKillBullet bid)})         = s{allBullets  = filterIL ((/= bid) . bulletID) $ allBullets s}
updateServerState (s, Event ServerInput{msg=(pid, CSMsgDeath h)})              = s{allPlayers = reInitDead pid (allPlayers s)}
    where reInitDead pid (p:ps) = if playerID p == pid then (initializePlayer pid (playerName p)):ps else p:reInitDead pid ps
          reInitDead _ _ = []
updateServerState (s, Event ServerInput{msg = (_, CSMsgExit exitPlayerName), handle = Just hand})  =
    let newHandles  = filter (\(pid, h) -> h /= hand)  $ handles s
        -- partition :: (a -> Bool) -> [a] -> ([a], [a])
        (newPlayers, [exitPlayer])  =  partition (\p -> playerName p /= exitPlayerName) $ allPlayers s
    in s{allPlayers = newPlayers, handles = newHandles, lastExitID = playerID exitPlayer}
updateServerState (s, Event ServerInput{msg=(_, CSMsgJoin name),handle=Just hand})                 =
    let pid       = nextID s
        newPlayer = initializePlayer pid name
    in s{handles  = handles s ++ [(pid,hand)], nextID = pid+1, allPlayers = allPlayers s ++ [newPlayer]}
updateServerState (s, Event ServerInput{msg=(_, CSMsgBulletHole bh)}) = s
updateServerState (s, NoEvent) = s
updateServerState (s, si)       = error $ "updateServerState couldn't find a match for " ++ (show si)


initializePlayer :: ID -> String -> Player
initializePlayer pid name = Player {playerID = pid,
                                    playerPos = Vec3d (25*(fromIntegral pid),0,10),
                                    playerVel = Vec3d (0,0,0),
                                    playerAcc = Vec3d (0,0,0),
                                    playerView = (0,0),
                                    playerRadius = 15,
                                    playerLife = maxLife,
                                    playerColor = let pid' = fromIntegral (pid+2)
                                                  in Vec3d (0.5, 0.2*pid' - (fromIntegral $ floor $ 0.2*pid'), 0.1*pid' - (fromIntegral $ floor $ 0.1*pid')),
                                    playerName = name}

moveObjs :: (ServerState -> IL a) -> (a -> Position3) -> (a -> Velocity3) -> SF ServerState (IL Position3)
moveObjs listFun posFun velFun = proc s0 -> do
    dPs <- integral -< fmap velFun $ listFun s0
    returnA -< (fmap posFun $ listFun s0) ^+^ dPs

checkHits :: (ServerState, ServerState) -> [Hit]
-- catMaybes :: [Maybe a] -> [a]
checkHits (sprev, s) = catMaybes $ map collisionLP [(lprev,l,p) | (lprev,l) <- map snd $ assocsIL $
                                                                     zipWithIL (\a b -> (a,b)) (const Nothing) (const Nothing)
                                                                               (allBullets sprev) (allBullets s),
                                                                  p <- allPlayers s, bulletpID l /= playerID p]


-- checkWallHits :: (ServerState, ServerState) -> [BulletHole]
-- checkWallHits (sprev, s) = catMaybes $ map collisionPoint [(lprev,l,p) | (lprev,l) <- map snd $ assocsIL $
--                                                                             zipWithIL (\a b -> (a,b)) (const Nothing) (const Nothing)
--                                                                                       (allBullets sprev) (allBullets s),
--                                                                          p <- planes]


toMessages :: (ServerState, Event ServerInput, [Hit]) -> [SCMsg]
toMessages (s, esi, hits) =
    let allIDs = map playerID $ allPlayers s
        playerUpdates = event []
                        (\si -> case msg si of -- player updates (exclude sender from recips, colliding players from list)
                            (pid, CSMsgPlayer p) -> [(i, SCMsgPlayer p) | i <- allIDs, i /= pid]
                            (pid, CSMsgDeath h) -> let killed = case find ((pid ==) . playerID) (allPlayers s) of
                                                                            Nothing -> error "Couldn't find a player that was just killed...???"
                                                                            Just p -> p
                                                   in [(i, SCMsgSpawn (PlayerObj killed)) | i <- allIDs, i /= playerID killed] ++
                                                      [(pid, SCMsgInitialize killed)]
                            -- 新加入的 newPlayer 放在列表最后
                            (_, CSMsgJoin _) -> let pl = head $ reverse $ allPlayers s
                                                  in [(playerID pl, SCMsgInitialize pl)] ++
                                                     [(playerID pl, SCMsgSpawn (PlayerObj p)) | p <- allPlayers s, playerID p /= playerID pl] ++
                                                     [(i, SCMsgSpawn (PlayerObj pl)) | i <- allIDs, i /= playerID pl]
                            -- Send SCMsgExit so client can delete the object?
                            (_, CSMsgExit name) -> [(i, SCMsgRemove (lastExitID s)) | i <- allIDs]

                            _ -> []) esi
        holeUpdates  = event []
                       (\si -> case msg si of
                                    (pid, CSMsgBulletHole bh) -> [(i, SCMsgBulletHole bh ) | i <- allIDs, i /= pid]
                                    _ -> [] ) esi
     in (playerUpdates) ++ [(i, SCMsgHit h) | i <- allIDs, h <- hits] -- hit broadcasts
                        ++ holeUpdates


sendAll :: ServerState -> [SCMsg] -> IO()
-- foldl' :: (b -> a -> b) -> b -> t a -> b ： strict
sendAll s msgs = do
                 case length msgs of 
                      0 -> return ()
                      _ -> print $ length msgs
                 mapM_ (sendMsg) msgs
                 where sendMsg (ident,msg) = do
                                          -- find :: Foldable t => (a -> Bool) -> t a -> Maybe a
                                            let handle = find (\(hi,h) -> ident == hi ) (handles s)
                                            sendSCMsg handle (ident, msg)

  -- fst $ foldl' (\(io,hndls) msg -> let (io',hndls') = sendMsg (msg,hndls)
  --                                                 in (io >> io', hndls'))
  --                            (return (),handles s) (msgs)
  --   where
  --       sendMsg ((ident,msg),(hi,h):hndls) = do find (collidesWith (ooBounds oo) . ooBounds)
  --                                                    (map snd $ assocsIL $ deleteIL k oos)

  --                                             case (ident == hi) of
  --                                                True -> (sendSCMsg h (ident,msg), (hi,h):hndls)
  --                                                -- 这里貌似有 bug，handle 顺序必须和 message 顺序相同，需要替换为寻找
  --                                                False -> sendMsg ((ident,msg),hndls)  
  --       sendMsg (_,[]) = (return (), [])

sendSCMsg :: Maybe (ID, Handle) -> SCMsg -> IO ()
sendSCMsg hd msg = case hd of 
                    Just (hi,h) -> do
                                   print "SC message: "
                                   print $ stringify msg
                                   hPutStrLn h $ stringify msg
                                   hFlush h
                    _ -> return ()

-- sendSCMsg :: Handle -> SCMsg -> IO ()
-- sendSCMsg h msg = do
--     --_ <- hIsOpen h
--     print $ stringify msg
--     hPutStrLn h $ stringify msg -- debugShow (stringify msg)
--     hFlush h
