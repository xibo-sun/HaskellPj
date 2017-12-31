module Main where

import FRP.Yampa
import Graphics.UI.WX

import RunGame
import Terrain
import Render
import Object
import Common

main :: IO()
main = start gui

gui :: IO()
gui = do f <- frame [text := "Lambda CS Game",
                     clientSize := sz 1000 625]
         p <- panel f [clientSize := sz 1000 625,
                       position := pt 0 0]

         image <- imageCreateFromFile "tga/background.png"
         imagep <- panel p [ on paint := onPaint image]

         quit <- bitmapButton f [picture := "tga/quit.bmp", 
                                 on command := close f,
                                 position := pt 810 106]
         singlePlayer <- bitmapButton f [picture := "tga/SinglePlayer.bmp" 
                                         ,on command := do 
                                                          close f
                                                          main'
                                         ,position := pt 810 10]
         multiPlayer  <- bitmapButton f [picture := "tga/MultiPlayer.bmp", 
                                         on command := close f,
                                         position := pt 810 58]

         set f [layout := fill $ container p $ fill $ widget imagep
                ,outerSize := sz 1000 625]
   
         where
           onPaint image dc rect = drawImage dc image pointZero []

main' :: IO ()
main' = do
    gameInit
    runGame defaultConfigs Nothing (game [terrain0,  observer dummyPlayer] >>>
           arr (\(ooss,msgs) -> (renderObsObjStates ooss, return ())))
  where renderObsObjStates = foldl (\io oos -> io >> renderObsObjState oos) (return ())

-- arr ： 将一个 (b->c) 的转换函数塞到 arrow 中
-- arr 和 >>> 在 FRP.Yampa.Core 中，
-- SF instance 定义如下：
-- instance Arrow SF where
--    arr    = arrPrim
--    first  = firstPrim
--    second = secondPrim
--    (***)  = parSplitPrim
--    (&&&)  = parFanOutPrim
--
-- #if __GLASGOW_HASKELL__ >= 610
-- #else
--    (>>>)  = compPrim
-- #endif
--
-- 其中， compPrim :: SF a b -> SF b c -> SF a c
--       arrPrim :: (a -> b) -> SF a b
