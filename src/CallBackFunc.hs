{-# LANGUAGE PackageImports #-}

module CallBackFunc where


import Data.IORef
import Control.Concurrent
import Control.Monad (when)
import qualified Graphics.Rendering.OpenGL as GL
import qualified "GLFW" Graphics.UI.GLFW   as GLFW

import Common

-- 外界输入
data GameInput = GameInput {key :: Maybe GLFW.Key,                   -- 按键值
                            keyState :: Maybe GLFW.KeyButtonState,   -- 按键状态
                            leftClick :: Bool,                       -- 鼠标左键
                            rightClick :: Bool,                      -- 鼠标右键
                            posMouse :: GL.Position,
                            message :: SCMsg}                 -- 鼠标位置
    deriving Show


-- 定义信道反应函数
type ReactChan a = Chan (a -> a)

addToReact :: ReactChan a -> (a -> a) -> IO ()
addToReact rch f  = writeChan rch f

getReactInput :: ReactChan a -> a -> IO a
getReactInput rch old = do
    f <- readChan rch
    return $ f old


-- 鼠标状态更新时间
mouseTimer :: Double
mouseTimer = 0.001

-- 按键回调函数
keyboardCallback :: ReactChan GameInput -> IORef Bool -> GLFW.Key -> GLFW.KeyButtonState -> IO ()
keyboardCallback _ quit (GLFW.SpecialKey GLFW.ESC) GLFW.Press = writeIORef quit True
keyboardCallback rch _ k ks = addToReact rch (\gi -> gi {key = Just k, keyState = Just ks})

-- 鼠标按键回调函数
mouseClickCallback :: ReactChan GameInput -> GLFW.MouseButton -> GLFW.KeyButtonState -> IO ()
mouseClickCallback rch GLFW.ButtonLeft ks = addToReact rch (\gi -> gi {leftClick = (ks == GLFW.Press)})
mouseClickCallback rch GLFW.ButtonRight ks = addToReact rch (\gi -> gi {rightClick = (ks == GLFW.Press)})
mouseClickCallback rch _ _ = addToReact rch (\gi -> gi)

-- 鼠标移动回调函数
mouseMotionCallback :: ReactChan GameInput -> IORef Double -> IORef Int -> GL.Position -> IO ()
mouseMotionCallback rch timerRef yPrev (GL.Position x y) = do
    t' <- readIORef timerRef
    t <- GL.get GLFW.time
    when (t-t' >= mouseTimer) $ do
        yp' <- readIORef yPrev
        let yp = fromIntegral yp'
            p = GL.Position (if x < (width `div` 4) then x+(width `div` 2) else if x >= (3*width `div` 4) then x - (width `div` 2) else x)
                         (if y < (height `div` 3) then (height `div` 3) else if y >= (2*height `div` 3) then (2*height `div` 3 - 1) else y)
            needsUpdate = x < width `div` 4 || x >= 3*width `div` 4 || (y < height `div` 3 && y > yp) || (y >= 2*height `div` 3 && y < yp)
                                    || y < height `div` 6 || y >= 5 * height `div` 6
            performUpdate = GLFW.mousePos GL.$= p
        writeIORef yPrev $ fromIntegral y
        when needsUpdate performUpdate
        addToReact rch (\gi -> gi {posMouse = p})
    writeIORef timerRef t

