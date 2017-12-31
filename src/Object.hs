{-# LANGUAGE Arrows #-}
{-# LANGUAGE PackageImports #-}

module Object where


import FRP.Yampa
import Graphics.Rendering.OpenGL
import "GLFW" Graphics.UI.GLFW hiding (time)
import Data.Maybe

import Common
import CallBackFunc
import CollisionTest
import Render
import Vec3d
import Terrain
import Net

type ObjectSF = SF ObjInput ObjOutput

data ObjInput = ObjInput {
    oiGameInput :: !GameInput,
    oiColliding :: !(Maybe ObjOutput)

}

data ObjOutput = ObjOutput {
    ooObsObjState :: !ObsObjState,
    ooKillReq :: !(Event ()),
    ooNetworkMsgs :: ![CSMsg],             
    ooSpawnReq :: ![ObjectSF],             
    ooBounds :: !BoundingVolume   
}

instance Eq ObjOutput where
    (==) _ _ = error "(==) called on ObjOutputs"
    (/=) _ _ = error "(/=) called on ObjOutputs"

data ObsObjState = OOSSelf !Player
                 | OOSTerrain ![TerrainElement]
                 | OOSBullet !Bullet
                 | OOSPlayer !Player
                 | OOSBulletHole !BulletHole
                 | OOSNone
    deriving Show


renderObsObjState :: ObsObjState -> IO ()
renderObsObjState (OOSSelf p) = renderSelf p
renderObsObjState (OOSTerrain ts) = (foldr (>>) (return ()) $ map renderTerrainElement ts)
renderObsObjState (OOSBullet b) = renderBullet b
renderObsObjState (OOSBulletHole bh) = renderBulletHole bh
renderObsObjState (OOSPlayer p) = renderPlayer p
renderObsObjState OOSNone = return ()


terrain0 :: ObjectSF
terrain0 = proc oi -> do 
    returnA -< ObjOutput {ooObsObjState = OOSTerrain defTerrain,
                          ooKillReq = NoEvent,
                          ooNetworkMsgs = [],
                          ooSpawnReq = [],
                          ooBounds = getTerrainBounds defTerrain}


observer :: Player -> ObjectSF
observer pl = let setFromKey k (gi, prev) = dup $ case (key gi == Just k, keyState gi) of
                                                            (True,Just Press) -> 1
                                                            (True,Just Release) -> 0
                                                            (_   ,_        ) -> prev
                  speed = 80
                  getx GameInput{posMouse=Position x y} = x
                  gety GameInput{posMouse=Position x y} = y

                  powerupSF' val = switch (arr ((+val) . fst) &&& after 15 ()) (\_ -> powerupSF)
                  powerupSF = switch (arr fst &&& arr snd) powerupSF'

               in proc (ObjInput {oiGameInput = gi, oiColliding = collider}) -> do
                        -- theta : 绕z轴旋转角度，  sensitivity：灵敏度
                    let theta = sensitivity * (fromIntegral $ getx gi - (width `div` 2))
                        -- phi   : 绕y轴旋转角度
                        phi = (-sensitivity) * (fromIntegral $ gety gi - (height `div` 2))
                        -- (x轴移动，y轴移动，z轴移动)
                        f' = Vec3d (cos theta * cos phi, -sin theta * cos phi,0)--, sin phi)
                        r' = Vec3d (-cos (theta-pi/2),sin (theta-pi/2), 0)
                        b' = Vec3d (cos theta * cos phi, -sin theta * cos phi, sin phi)
                        view = (theta, phi)
                        f = speed *^ f'
                        b = speed *^ b'
                        r = speed *^ r'
                        
                    -- loopPre :: c -> SF (a, c) (b, c) -> SF a b
                    -- (gi, x) -> (x, x) ==> gi -> x

                    -- (<<^) :: Arrow a => a c d -> (b -> c) -> a b d
                    -- edge :: SF Bool (Event ()) : A rising edge detector.
                    fwd   <- loopPre 0 $ arr $ setFromKey (CharKey 'W') -< gi
                    bwd   <- loopPre 0 $ arr $ setFromKey (CharKey 'S') -< gi
                    right <- loopPre 0 $ arr $ setFromKey (CharKey 'D') -< gi
                    left  <- loopPre 0 $ arr $ setFromKey (CharKey 'A') -< gi
                    collideEvent <- edge <<^ (/= Nothing) -< collider




                    -- isEvent :: Event a -> Bool
                    let df = (fwd - bwd) *^ f
                        dr = (right - left) *^ r
                        a = df ^+^ dr
                        v = (if isEvent collideEvent then -6 else 1) *^ a -- 碰撞检测，貌似有问题
                    
                    -- v <- integral -< a
                    -- integral :: VectorSpace a s => SF a a
                    -- (^<<) :: Arrow a => (c -> d) -> a b c -> a b d
                    -- tag :: Event a -> b -> Event b
                    -- event :: a -> (b -> a) -> Event b -> a
                    p <- (playerPos pl ^+^) ^<< integral -< v

                    -- time :: SF a Time
                    
                    
                    t <- time -< ()
                    let bul = Bullet {bulletID = round (10*t),
                                      bulletpID = playerID pl,
                                      bulletPos = p,
                                      bulletVel = 50 *^ b}

                    fire <- edge -< leftClick gi

                    -- edgeBy :: (a -> a -> Maybe b) -> a -> SF a (Event b)
                    -- 跳变检测器（输入为 a 的状态）
                    -- (a -> a -> Maybe b) 为跳变检测函数
                    -- a 初始状态

                    changeVel <- edgeBy (\prev cur -> if prev /= cur then Just () else Nothing) zeroVector -< v
                    changeView <- edgeBy (\prev cur -> if prev /= cur then Just () else Nothing) (0,0) -< view
                    changeMsg <- edgeBy (\prev cur -> if prev /= cur then Just cur else Nothing) dummySCMsg -< message gi
                    let hitEvent = case changeMsg of
                                 Event (_,SCMsgHit h) -> if playerID pl == player2ID h then changeMsg else NoEvent
                                 _ -> NoEvent
                        holeEvent = case changeMsg of
                                 Event (_,SCMsgBulletHole bh) -> if playerID pl /= holepID bh then Event (bh) else NoEvent
                                 _ -> NoEvent
    

                    life <- loopPre (playerLife pl) (arr (\(hev,life) -> dup $ event life (\(_,SCMsgHit h) -> life - 20) hev)) -< hitEvent
                    let killerHit = case hitEvent of
                                         Event (_,SCMsgHit h) -> Just h
                                         _ -> Nothing

                        kill = if life <= 0 then Event (fromJust killerHit) else NoEvent

                     


                    let pl' = pl {playerPos = p,
                                  playerVel = v,
                                  playerAcc = a,
                                  playerView = (theta,phi),
                                  playerLife = life,
                                  playerRadius = 8}
                    returnA -< ObjOutput {ooObsObjState = OOSSelf pl',
                                          ooKillReq = kill `tag` (),
                                          ooNetworkMsgs = (\xs -> [x | Event x <- xs]) $
                                                             [fire `tag` (playerID pl, CSMsgBullet bul),
                                                             foldl (mergeBy const) NoEvent [changeView, changeVel, hitEvent `tag` ()] `tag` (playerID pl, CSMsgPlayer pl'),
                                                             fmap (\hit -> (playerID pl, CSMsgDeath hit)) kill],
                                          ooSpawnReq = (event [] (\_ -> [bullet bul]) fire) ++ (event [] (\bh -> [bulletHole bh]) holeEvent),
                                          ooBounds = let v = Vec3d (playerRadius pl', playerRadius pl', playerRadius pl')
                                                     in BoundingBox (p ^-^ v) (p ^+^ v)}


serverObject :: RandomGen g => g -> String -> ObjectSF
serverObject g playerNameStr = proc (ObjInput {oiGameInput = gi}) -> do
    changeMsg <- edgeBy (\prev cur -> if prev /= cur then Just cur else Nothing) dummySCMsg -< message gi
    let processSCMsg NoEvent = ObjOutput {ooObsObjState = OOSNone, ooNetworkMsgs = [], ooKillReq = NoEvent, ooSpawnReq = [], ooBounds = BoundingEmpty}
        processSCMsg (Event (_,msg)) = ObjOutput {ooObsObjState = OOSNone, ooNetworkMsgs = [],
                                                  ooKillReq = NoEvent,
                                                  ooSpawnReq = case msg of
                                                                   SCMsgSpawn (PlayerObj p) -> [player p dummySCMsg]
                                                                   SCMsgInitialize p -> [observer p{playerName = playerNameStr}]
                                                                   _ -> [],
                                                  ooBounds = BoundingEmpty}
    let oo = processSCMsg changeMsg
    returnA -< oo

player :: Player -> SCMsg -> ObjectSF
-- switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
-- 基本开关： 默认使用第一个 SF，当 Event C 发生时，则应用第二个 SF
-- 这里表示在多个 player 之间切换操作
player pl initMsg = switch (player' pl initMsg) (\(p,msg) -> player p msg)

player' :: Player -> SCMsg -> SF ObjInput (ObjOutput, Event (Player, SCMsg))
player' pl initMsg = proc ObjInput{oiGameInput=gi} -> do
    --changeMsg <- loopPre initMsg detectChangeSF -< message gi
    changeMsg <- edgeBy (\prev cur -> if prev /= cur then Just cur else Nothing) initMsg -< message gi
    let update = event NoEvent (\(i,msg') -> case msg' of
                                                      SCMsgPlayer p -> if playerID pl == playerID p then Event p else NoEvent
                                                      _ -> NoEvent) changeMsg
        kill = event NoEvent (\(i,msg') -> case msg' of
                                                    SCMsgSpawn (PlayerObj p) -> if playerID pl == playerID p then Event () else NoEvent
                                                    _ -> NoEvent) changeMsg
        exit = event NoEvent (\(i,msg') -> case msg' of
                                                    SCMsgRemove pID -> if playerID pl == pID then Event() else NoEvent
                                                    _ -> NoEvent) changeMsg
    pos <- (playerPos pl ^+^) ^<< integral -< playerVel pl
    let pl' = pl {playerPos = pos}
        rad = playerRadius pl
    returnA -< (ObjOutput {ooObsObjState = OOSPlayer pl', -- {playerPos = pos},
                           ooNetworkMsgs = [],
                           ooKillReq = kill `lMerge` exit, -- left-biased merge
                           ooSpawnReq = [],
                           ooBounds = let du = Vec3d (rad, rad, rad)
                                          dd = Vec3d (rad, rad, rad * 4)
                                      in BoundingBox (pos ^-^ dd) (pos ^+^ du)},
                -- fmap f (Event a) = Event (f a)
                fmap (\ev -> (ev,message gi)) update)



bullet :: Bullet -> ObjectSF
bullet b = proc (ObjInput {oiGameInput = gi, oiColliding = collider}) -> do
            p <- ((bulletPos b) ^+^) ^<< integral -< bulletVel b
            let b' = b {bulletPos = p}
            -- repeatedly :: Time -> b -> SF a (Event b)
            -- mergeBy :: (a -> a -> a) -> Event a -> Event a -> Event a
                bulletHoles = catMaybes $ map collisionPoint [(b, b', pla) | pla <- planes ] 
                collited = case (null bulletHoles) of
                                           True -> NoEvent
                                           False -> Event ()
            
            kill <- repeatedly 3 () -< ()
            -- changeMsg <- edgeBy (\prev cur -> if prev /= cur then Just cur else Nothing) dummySCMsg -< message gi
            -- let bulletHoleEvent = case changeMsg of
            --                       Event (_,SCMsgBulletHole bh) -> Event (bh)
            --                       _ -> NoEvent

            
            -- collited <- edge <<^ (/= Nothing) -< collider
            -- let bh = BulletHole { holePos = p }

            returnA -< ObjOutput {ooObsObjState = OOSBullet b',
                                  ooKillReq = mergeBy (\_ _ -> ()) kill collited,
                                  ooNetworkMsgs = event [] (\_ -> [(bulletpID b, CSMsgKillBullet $ bulletID b)] ++
                                                                   [(bulletpID b, CSMsgBulletHole $ bh) | bh <- bulletHoles])
                                                           (mergeBy (\_ _ -> ()) kill collited),
                                  ooSpawnReq = event [] (\_ -> [bulletHole bh | bh <- bulletHoles]) collited,
                                  ooBounds   = BoundingEmpty }
                                  --ooBounds = BoundingBox (p ^-^ Vec3d(bulletRadf, bulletRadf, 0)) (p ^+^ Vec3d(bulletRadf, bulletRadf, bulletHeightf)) }


bulletHole :: BulletHole -> ObjectSF
bulletHole h = proc gi -> do
            returnA -< ObjOutput {ooObsObjState = OOSBulletHole h,
                                  ooKillReq = NoEvent,
                                  ooNetworkMsgs = [],
                                  ooSpawnReq = [],
                                  ooBounds = BoundingEmpty }
