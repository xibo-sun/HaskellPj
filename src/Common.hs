{-# LANGUAGE PackageImports #-}

module Common where


import Data.IORef
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import FRP.Yampa
import qualified Graphics.Rendering.OpenGL as GL
import qualified "GLFW" Graphics.UI.GLFW   as GLFW

import Vec3d

--------------------------------------------------------
--------------------------------------------------------
---
---  游戏基本数据
---
--------------------------------------------------------
--------------------------------------------------------

-- 游戏设定
data GameConfig = GameConfig {
    gcPlayerName :: String,
    gcTracker :: String}

-- 游戏基本数据
data GameData = GameData {
    startTime    :: IORef Double,
    lastDrawTime :: IORef Double,
    numFrames    :: IORef Int,
    singerPlay   :: IORef Bool,
    multiPlay    :: IORef Bool
}

-- 纹理图片数据
data GLTexture = GLTexture {
  texSky  :: GL.TextureObject,
  texFloor :: GL.TextureObject,
  texWall  :: GL.TextureObject,
  texWall2 :: GL.TextureObject,
  texGun   :: GL.TextureObject,
  texPlayerHead :: GL.TextureObject,
  texPlayerBody :: GL.TextureObject,
  texBulletHole :: GL.TextureObject
}


--------------------------------------------------------
--------------------------------------------------------
---
---  玩家数据
---
--------------------------------------------------------
--------------------------------------------------------
type ID = Int
type Position3 = Vec3d
type Velocity3 = Vec3d
type Acceleration3 = Vec3d
type Color3 = Vec3d

-- 玩家基本数据
data Player = Player {
    playerID :: !ID,
    playerPos :: !Position3,
    playerVel :: !Velocity3,
    playerAcc :: !Acceleration3,
    playerView :: !(Float,Float),
    playerRadius :: !Float,
    playerLife :: !Float,
    playerColor :: !Common.Color3,
    playerName :: !String
}
    deriving (Show, Eq)

data Bullet = Bullet {
    bulletID  :: !Int,
    bulletpID :: !Int,
    bulletPos :: !Position3,
    bulletVel :: !Velocity3
}
    deriving (Show, Eq)

data BulletHole = BulletHole{
    holepID :: !Int,
    holePos :: !Position3,
    holeVec :: !Velocity3
}
    deriving (Show, Eq)

data Hit = Hit {
    player1ID :: !ID,
    player2ID :: !ID,
    hitBulletID :: !ID
}
    deriving (Show, Eq)

data Plane = Plane {
     planeVec :: !Velocity3,
     planePoint :: !Position3,
     planeRange :: !(GL.GLfloat, GL.GLfloat, GL.GLfloat, GL.GLfloat, GL.GLfloat, GL.GLfloat)
}
    deriving (Show, Eq)
--------------------------------------------------------
--------------------------------------------------------
---
---  基本函数
---
--------------------------------------------------------
--------------------------------------------------------

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return

maxLife :: Float
maxLife = 100

loadTexture :: String -> IO GL.TextureObject
loadTexture filename = do
  dataFileName <- getDataFileName filename
  [texName] <- GL.genObjectNames 1
  GL.textureBinding GL.Texture2D GL.$= Just texName
  GLFW.loadTexture2D dataFileName [GLFW.BuildMipMaps]
  GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Just GL.Linear'), GL.Linear') -- trilinear filtering
  return texName

-- 得到纹理数据
initTextureData :: IO GLTexture
initTextureData = do
  -- 加载纹理图片
  texSky   <- loadTexture "tga/sky.tga"
  texFloor <- loadTexture "tga/floor.tga"
  texWall  <- loadTexture "tga/wall.tga"
  texWall2 <- loadTexture "tga/wall2.tga"
  texGun   <- loadTexture "tga/gun.tga"
  texPlayerHead <- loadTexture "tga/Head.tga"
  texPlayerBody <- loadTexture "tga/Body.tga"
  texBulletHole <- loadTexture "tga/BulletHole.tga"

  -- 返回纹理数据
  return $ GLTexture {
    texSky = texSky,
    texFloor = texFloor,
    texWall = texWall,
    texWall2 = texWall2,
    texGun   = texGun,
    texPlayerHead = texPlayerHead,
    texPlayerBody = texPlayerBody,
    texBulletHole = texBulletHole
  }

textures :: GLTexture
textures = unsafePerformIO (initTextureData)


dummyPlayer :: Player
dummyPlayer = Player {playerID = 0,
                      playerPos = zeroVector,
                      playerVel = zeroVector,
                      playerAcc = zeroVector,
                      playerView = (0,0),
                      playerRadius = 8,
                      playerLife = maxLife,
                      playerColor = Vec3d(0.5, 0.2, 0.7),
                      playerName = "Dummy"}



defaultConfigs :: GameConfig
defaultConfigs = GameConfig {
    gcPlayerName = "uninitialized",
    gcTracker    = "test"
}

width, height :: GL.GLint
(width,height) = (640, 480)

widthf, heightf :: GL.GLdouble
widthf = fromRational $ toRational width
heightf = fromRational $ toRational height

widthfl, heightfl :: GL.GLfloat
widthfl = fromRational $ toRational width
heightfl = fromRational $ toRational height

centerCoordX, centerCoordY :: Float
centerCoordX = fromIntegral width / 2
centerCoordY = fromIntegral height / 2


bulletRad, bulletHeight :: GL.GLdouble
bulletRad = 0.5
bulletHeight = 10

bulletRadf, bulletHeightf :: Float
bulletRadf = float bulletRad
bulletHeightf = float bulletHeight

sensitivity :: Float
sensitivity = pi/(fromIntegral $ width `div` 4)

-- 初始化玩家视角

-- lookAt : (相机在世界中的坐标，相机对准的方向，相机的法向量)
initFrustum :: IO ()
initFrustum = do
    GL.loadIdentity
    let near   = 0.8                                          -- 近点
        far    = 3000                                         -- 远点
        right  = 0.4
        top    = 0.3
    GL.frustum (-right) right (-top) top near far                -- 产生投影视角
    -- TODO: explain this
    GL.lookAt (GL.Vertex3 0 0 0) (GL.Vertex3 1 0 0) (GL.Vector3 0 0 1)    -- 设定玩家视角


-- 地形信息
planes :: [Plane]
planes = [Plane {planeVec = Vec3d(0,0,1), planePoint = Vec3d(470,0,-48), planeRange = (-30,970,-500,500,-48,-48)},
          Plane {planeVec = Vec3d(0,-1,0), planePoint = Vec3d(470,500,152), planeRange = (-30,970,500,500,-48,152)}]


-- 网络部分

data Obj = PlayerObj !Player
         | BulletObj !Bullet
    deriving (Show, Eq)

data SCMsg' = SCMsgInitialize !Player    -- To initiatiate the joining player
            | SCMsgPlayer !Player
            | SCMsgHit !Hit
            | SCMsgBulletHole !BulletHole
            | SCMsgSpawn !Obj
            | SCMsgRemove !Int
    deriving (Show, Eq)

data CSMsg' = CSMsgPlayer !Player        -- Send when velocity changes
            | CSMsgUpdate !Player
            | CSMsgBullet !Bullet          -- Send when a laser is shot by client
            | CSMsgKillBullet !ID
            | CSMsgBulletHole !BulletHole
            | CSMsgDeath !Hit
            | CSMsgExit !String 
            | CSMsgJoin !String
    deriving (Show, Eq)

type SCMsg = (ID, SCMsg')   -- Server to Client, i.e. runs on Client
type CSMsg = (ID, CSMsg')   -- Client to Server, i.e. runs on Server

printFlush :: String -> IO ()
printFlush s = do
    print s
    -- hFlush :: Handle -> IO ()
    -- The action hFlush hdl causes any items buffered for output in handle hdl to be sent immediately to the operating system.
    hFlush stdout
    hFlush stderr

