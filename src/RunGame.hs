{-# LANGUAGE Arrows #-}
{-# LANGUAGE PackageImports #-}

module RunGame where


import FRP.Yampa
import Data.IORef
import Control.Monad.RWS.Strict (liftIO)
import Control.Monad (unless, when)
import Control.Concurrent
import Control.Exception
import Data.Time.Clock
import System.IO                    -- Handle
import Text.Printf (printf)
import qualified Graphics.Rendering.OpenGL as GL
import qualified "GLFW" Graphics.UI.GLFW   as GLFW

import CallBackFunc
import Object
import GameCore 
import Common
import Collection
import Render
import Net


redrawTimer :: Double
redrawTimer = 0.005

game :: [ObjectSF] -> SF GameInput ([ObsObjState], [CSMsg])
game initialObjs = proc gi -> do
    oos <- loopPre emptyIL (arr dup <<< gameCore (listToIL initialObjs)) -< gi
    -- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
    returnA -< (map ooObsObjState $ elemsIL oos, concatMap ooNetworkMsgs $ elemsIL oos)

runGame :: GameConfig -> Maybe Handle -> SF GameInput (IO (), IO ()) -> IO ()
runGame GameConfig{gcPlayerName=playerName} handle gameSF = do
    t <- GL.get GLFW.time
    sTime <- newIORef t                               -- 开始时间
    ldTime <- newIORef t                              -- 上一帧绘制时间
    yPrev <- newIORef (fromIntegral $ height `div` 2) 
    nFrames <- newIORef 0                             -- 起始帧率

    let gd = GameData {startTime = sTime,
                       lastDrawTime = ldTime,
                       numFrames = nFrames}

    -- 新建一个信道。（并发执行的子线程将结果存入信道）
    rch <- newChan
    -- 初始化一个反应处理器，接受输入信号，并进行一定操作
    {- type ReactHandle a b = IORef (ReactState a b)

       reactInit :: IO a -> (ReactHandle a b -> Bool -> b -> IO Bool) -> SF a b -> IO (ReactHandle a b)
       -- IO a ： 输入信号
       -- (ReactHandle a b -> Bool -> b -> IO Bool) ： 激励函数，将输入送入信号函数
       -- SF a b ：信号函数 ，a 代表输入信号，b 代表输出信号
       -- 在这里 b 为一系列 IO() 操作
    -}
    rh <- reactInit (return initGameInput) (actuate gd) gameSF

    tm <- newIORef t
    quit <- newIORef False

    -- 网络初始化
    networkInit rch handle
    

    -- 设置响应函数
    GLFW.keyCallback GL.$= keyboardCallback rch quit
    GLFW.mouseButtonCallback GL.$= mouseClickCallback rch
    GLFW.mousePosCallback GL.$= mouseMotionCallback rch tm yPrev
    GLFW.windowCloseCallback GL.$= do 
                writeIORef quit True
                return True

    -- 关闭自动事件检查
    GLFW.disableSpecial GLFW.AutoPollEvent
    startTime <- getCurrentTime
    
    -- 游戏主循环
    loop rh rch quit startTime initGameInput
    

    -- 关闭窗口
    GLFW.closeWindow
    GLFW.terminate

    where
        -- 游戏主循环：
        -- rh    : 游戏事件处理器
        -- rch   : 信道
        -- quit  : 结束标志
        -- lTime : 上一循环事件
        -- curA  : 游戏数据

        -- react :: ReactHandle a b -> (DTime, Maybe a) -> IO Bool
        loop rh rch quit lTime curA = do
            GLFW.sleep 0.001
            GLFW.pollEvents
            curTime <- getCurrentTime
            let reactLoop cA = do
                empty <- isEmptyChan rch
                let dt = fromRational.toRational $ diffUTCTime curTime lTime
                if empty
                    then (react rh (dt, Nothing) >> return cA)
                    else (do
                        newA <- getReactInput rch cA
                        react rh (dt, Just newA)
                        reactLoop newA)
            nA <- reactLoop curA
            q <- readIORef quit
            unless q $ loop rh rch quit curTime nA

        -- 激励函数
        -- gd             : 游戏事件信息
        -- rch            : 信道
        -- shouldDraw     : 是否渲染
        -- renderActions  : 存储在 SF 中的渲染操作
        -- networkActions : 存储在 SF 中的网络操作
        actuate gd rch shouldDraw (renderActions, networkActions) = do
            st <- readIORef $ startTime gd
            ldt <- readIORef $ lastDrawTime gd
            t <- GL.get GLFW.time

            when (shouldDraw && t-ldt >= redrawTimer) $ do
                GL.clear [GL.ColorBuffer, GL.DepthBuffer]
                renderActions                   
                writeIORef (lastDrawTime gd) t
                renderText2D 5 30 (printf "FPS: %.4f" (1/(t-ldt))) 1
                GLFW.swapBuffers
            networkActions
            nf <- readIORef $ numFrames gd
            writeIORef (numFrames gd) (nf+1)
            return True

initGameInput :: GameInput
initGameInput = GameInput {key=Nothing, 
                           keyState=Nothing, 
                           leftClick=False,
                           rightClick = False,
                           posMouse=GL.Position 0 0,
                           message=dummySCMsg}


gameInit :: IO()
gameInit = do

  GLFW.initialize
  GLFW.openWindow (GL.Size width height) [GLFW.DisplayRGBBits 8 8 8,       -- RGB值位宽
                                     GLFW.DisplayAlphaBits 8,              -- 透明度位宽
                                     GLFW.DisplayDepthBits 24] GLFW.Window -- 深度值位宽

  GLFW.disableSpecial     GLFW.MouseCursor    -- 隐藏光标
  GLFW.swapInterval       GL.$= 1           -- 打开垂直同步，需要等待垂直同步信号才能开始渲染
  GLFW.windowTitle        GL.$= "Lambda CS"   -- 窗口标题
  GL.stencilTest        GL.$= GL.Enabled     -- 模板测试开启

  GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1.0    -- 清除窗口颜色
  GL.shadeModel GL.$= GL.Smooth                       -- 着色器模式
  -- 深度函数：小于替换
  GL.depthFunc GL.$= Just GL.Less
  -- 颜色混合模式：源颜色乘以自身的alpha 值，目标颜色乘以1.0减去源颜色的alpha值
  GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  -- 自动归一化法向量
  GL.normalize GL.$= GL.Enabled
  -- 着色器模型
  GL.shadeModel GL.$= GL.Smooth
  -- 初始化矩阵
  GL.matrixMode GL.$= GL.Projection                -- 投影矩阵
  initFrustum                                     
  GL.matrixMode GL.$= GL.Modelview 0               -- 模型矩阵
  GL.loadIdentity                               -- 加载单位矩阵

  GL.lighting GL.$= GL.Enabled                        -- 启用光照
  GL.light (GL.Light 0) GL.$= GL.Enabled                 -- 创建光源
  GL.diffuse (GL.Light 0) GL.$= GL.Color4 1 1 1 1        -- 漫反射光
  GL.position (GL.Light 0) GL.$= GL.Vertex4 1 1 1 0      -- 定向光源，所有光线几乎平行


-- 网络部分，监听服务器消息
networkInit :: ReactChan GameInput -> Maybe Handle -> IO ()
networkInit rch Nothing = return ()
networkInit rch (Just handle) = do
    fail <- hIsClosed handle
    when fail (error "networkInit handle fail")
    fid <- forkIO $ do
        let loop = do
             succ <- hWaitForInput handle (-1)
             when succ $ fetchSCMsg rch handle
             loop
        --loop
        catch loop (\e -> do let err = show (e :: IOException)
                             print ("Client network thread is dying.") 
                          )
    return ()