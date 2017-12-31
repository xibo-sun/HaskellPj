{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Net where

import System.IO

import Common
import CallBackFunc
import Vec3d


class Stringifiable a where
      stringify :: a -> String
      destringify :: String -> a

instance Stringifiable Bullet where

    stringify l = (show $ bulletID l) ++ delim ++
                  (show $ bulletpID l) ++ delim ++
                  (showVec3d $ bulletPos l) ++ delim ++
                  (showVec3d $ bulletVel l)
        where delim = ";"

    destringify s = let untildelim = span (/= ';')
                        (p1,s1) = untildelim s
                        (p2,s2) = untildelim $ drop 1 s1
                        (p3,s3) = untildelim $ drop 1 s2
                        (p4,_) = untildelim $ drop 1 s3
                     in Bullet {bulletID = read p1,
                               bulletpID = read p2,
                               bulletPos = readVec3d p3,
                               bulletVel = readVec3d p4}

instance Stringifiable Player where

    stringify p = (show $ playerID p) ++ delim ++
                  (showVec3d $ playerPos p) ++ delim ++
                  (showVec3d $ playerVel p) ++ delim ++
                  (showVec3d $ playerAcc p) ++ delim ++
                  (show $ playerView p) ++ delim ++
                  (show $ playerRadius p) ++ delim ++
                  (show $ playerLife p) ++ delim ++
                  (showVec3d $ playerColor p) ++ delim ++
                  (show $ playerName p)
        where delim = ";"

    destringify s = let untildelim = span (/= ';')
                        (p1,s1) = untildelim s
                        (p2,s2) = untildelim $ drop 1 s1
                        (p3,s3) = untildelim $ drop 1 s2
                        (p4,s4) = untildelim $ drop 1 s3
                        (p5,s5) = untildelim $ drop 1 s4
                        (p6,s6) = untildelim $ drop 1 s5
                        (p7,s7) = untildelim $ drop 1 s6
                        (p8,s8) = untildelim $ drop 1 s7
                        (p9,s9) = untildelim $ drop 1 s8
                    in Player {playerID = read p1,
                               playerPos = readVec3d p2,
                               playerVel = readVec3d p3,
                               playerAcc = readVec3d p4,
                               playerView = read p5,
                               playerRadius = read p6,
                               playerLife = read p7,
                               playerColor = readVec3d p8,
                               playerName = read p9}

instance Stringifiable Hit where

    stringify h = (show $ player1ID h)  ++ delim ++
                  (show $ player2ID h)  ++ delim ++
                  (show $ hitBulletID h)
        where delim = ";"

    destringify s = let untildelim = span (/= ';')
                        (p1,s1) = untildelim s
                        (p2,s2) = untildelim $ drop 1 s1
                        (p3,_) = untildelim $ drop 1 s2
                     in Hit {player1ID = read p1,
                             player2ID = read p2,
                             hitBulletID = read p3}

instance Stringifiable BulletHole where
    stringify bh = (show $ holepID bh) ++ delim ++
                   (showVec3d $ holePos bh) ++ delim ++ 
                   (showVec3d $ holeVec bh) ++ delim
            where delim = ";"

    destringify s  =  let untildelim = span (/= ';')
                          (p1,s1) = untildelim s
                          (p2,s2) = untildelim $ drop 1 s1
                          (p3,_)  = untildelim $ drop 1 s2
                       in BulletHole {holepID = read p1,
                                      holePos = readVec3d p2,
                                      holeVec = readVec3d p3}


instance Stringifiable Obj where

    stringify (PlayerObj p) = "player:" ++ (stringify p)
    stringify (BulletObj l) = "bullet:" ++ (stringify l)

    destringify s = let (p1,s1) = span (/= ':') s
                    in case p1 of
                          "player" -> PlayerObj $ destringify $ drop 1 s1
                          "bullet" -> BulletObj $ destringify $ drop 1 s1
                          _ -> error $ "Bad msg: can only be player or bullet, but is " ++ p1

instance Stringifiable SCMsg' where

    stringify (SCMsgInitialize p) = "initialize:" ++ (stringify p)
    stringify (SCMsgPlayer p) = "player:" ++ (stringify p)
    stringify (SCMsgHit h) = "hit:" ++ (stringify h)
    stringify (SCMsgBulletHole bh) = "bullethole:" ++ (stringify bh)
    stringify (SCMsgSpawn obj) = "spawn:" ++ (stringify obj)
    stringify (SCMsgRemove pID)   = "remove:" ++ (show pID)

    destringify s = let untildelim = span (/= ':')
                        (p1,s1) = untildelim s
                    in case p1 of
                          "initialize" -> SCMsgInitialize $ destringify $ drop 1 s1
                          "player"     -> SCMsgPlayer $ destringify $ drop 1 s1
                          "hit"        -> SCMsgHit $ destringify $ drop 1 s1
                          "bullethole" -> SCMsgBulletHole $ destringify $ drop 1 s1
                          "spawn"      -> SCMsgSpawn $ destringify $ drop 1 s1
                          "remove"     -> SCMsgRemove $ read $ drop 1 s1
                          _ -> error $ "Bad msg format for SCMsg': " ++ p1

instance Stringifiable CSMsg' where

    stringify (CSMsgPlayer p) = "player:" ++ (stringify p)
    stringify (CSMsgUpdate p) = "update:" ++ (stringify p)
    stringify (CSMsgBullet l) = "bullet:" ++ (stringify l)
    stringify (CSMsgKillBullet ident) = "killbullet:" ++ (show ident)
    stringify (CSMsgBulletHole bh) = "bullethole:" ++ (stringify bh)
    stringify (CSMsgDeath h) = "death:" ++ (stringify h)
    stringify (CSMsgJoin name) = "join:" ++ name
    stringify (CSMsgExit name) = "exit:" ++ name

    destringify s = let untildelim = span (/= ':')
                        (p1,s1) = untildelim s
                    in case p1 of
                          "player" -> CSMsgPlayer $ destringify $ drop 1 s1
                          "update" -> CSMsgUpdate $ destringify $ drop 1 s1
                          "bullet" -> CSMsgBullet $ destringify $ drop 1 s1
                          "killbullet" -> CSMsgKillBullet $ read $ drop 1 s1 -- TODO: read here is a bit unsafe
                          "bullethole" -> CSMsgBulletHole $ destringify $ drop 1 s1
                          "death"-> CSMsgDeath $ destringify $ drop 1 s1
                          "join" -> CSMsgJoin $ drop 1 s1
                          "exit" -> CSMsgExit $ drop 1 s1
                          _ -> error $ "Bad msg format for CSMsg': " ++ p1

instance Stringifiable SCMsg where

    stringify (ident,scmsg') = (show ident) ++ ":" ++ (stringify scmsg')
    destringify s = let (p1,s1) = span (/= ':') s
                    in (read p1, destringify $ drop 1 s1)

instance Stringifiable CSMsg where

    stringify (ident,csmsg') = (show ident) ++ ":" ++ (stringify csmsg')

    destringify s = let (p1,s1) = span (/= ':') s
                    in (read p1, destringify $ drop 1 s1)


fetchSCMsg :: ReactChan GameInput -> Handle -> IO ()
fetchSCMsg rch h = do
    ln <- hGetLine h
    print "SC message:"
    printFlush ln -- for debug
    addToReact rch (\gi -> gi {message = destringify ln})

-- Send msg from Client to Server
sendCSMsg :: Handle -> CSMsg -> IO ()
sendCSMsg h msg = do
    print "CS message: "
    print $ stringify msg
    hPutStrLn h (stringify msg)
    hFlush h -- required.

dummySCMsg :: SCMsg
dummySCMsg = (-1,SCMsgHit (Hit {player1ID= -1,player2ID= -1,hitBulletID= -1}))

dummyCSMsg :: CSMsg
dummyCSMsg = (-1,CSMsgExit "dummy")