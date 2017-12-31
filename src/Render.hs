{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}

module Render where

import FRP.Yampa
import Control.Monad (when)
import Graphics.Rendering.OpenGL as GL
import qualified "GLFW" Graphics.UI.GLFW as GLFW

import Vec3d
import Common


setVertex :: (TexCoord2 GLfloat, Vertex3 GLfloat) -> IO ()
setVertex (texCoordinates, vertexCoordinates) = do texCoord texCoordinates; vertex vertexCoordinates;


mapVerticies :: [(TexCoord2 GLfloat)] -> [(Vertex3 GLfloat)] -> IO ()
mapVerticies texs verts = mapM_ setVertex (zip texs verts)


renderText2D :: Float -> Float -> String -> Float -> IO ()
renderText2D x y str size =
    renderOrtho widthf heightf $ do
        GL.blend $= Enabled
        GL.blendFunc $= (SrcColor, OneMinusSrcAlpha)
        GL.textureFunction $= Replace       -- 纹理函数
        renderText x y str size
        GL.blend $= Disabled



-- sets up the orthographic mode so we can
-- draw at 2D screen coordinates
renderOrtho :: GLdouble -> GLdouble -> IO a -> IO()
renderOrtho width height graphicActions = do
   GL.matrixMode   $= Projection
   GL.unsafePreservingMatrix $ do
     GL.loadIdentity
     GL.ortho 0 width 0 height (-1) 1
     GL.matrixMode $= GL.Modelview 0
     graphicActions
     GL.matrixMode $= Projection
   GL.matrixMode   $= Modelview 0

--render text on 2D on front of screen
renderText :: Float -> Float -> String -> Float -> IO ()
renderText x y str s = GL.unsafePreservingMatrix $ do
    GL.translate (Vector3 x y (0::Float))
    GL.scale s s (1::Float)            -- 进行缩放
    GLFW.renderString GLFW.Fixed8x16 str


renderQuad ::  Maybe GL.TextureObject -> Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> IO ()
renderQuad mbTexObj p1 p2 p3 p4 = do
    texture Texture2D $= Enabled
    textureFunction $= Decal
    textureBinding Texture2D $= mbTexObj

    preservingMatrix $ do
    let verts = [p1, p2, p3, p4]
        (tx0, ty0) = (0.0, 0.0)
        (tx1, ty1) = (1.0, 1.0)
        texs = [(TexCoord2 tx0 ty1), (TexCoord2 tx0 ty0), (TexCoord2 tx1 ty0), (TexCoord2 tx1 ty1)]
    renderPrimitive Quads $ do mapVerticies texs verts
    texture Texture2D $= Disabled


renderBullet :: Bullet -> IO ()
renderBullet b = do
    loadIdentity
    preservingMatrix $ do
        translate $ vector3 $ bulletPos b 
        let drawDir = Vec3d(0,0,1)
            dir = FRP.Yampa.normalize (bulletVel b)
            rotAxis =  drawDir `cross` dir
            rotAngle = acos(dir .* drawDir) / pi * 180

        preservingMatrix $ do
            when (rotAxis /= zeroVector) $
                rotate rotAngle $ vector3 rotAxis -- 以 rotAxis 向量为轴旋转 rotAngle 角度
            --materialEmission FrontAndBack $= Color4 255 255 0 1
            --materialDiffuse FrontAndBack $= Color4 255 255 0 1
            let style = QuadricStyle (Just Smooth) NoTextureCoordinates Outside FillStyle
            renderQuadric style $ Cylinder 0.0 bulletRad bulletHeight 10 10

renderBulletHole :: BulletHole -> IO ()
renderBulletHole bh = do      
      let dirPlane = holeVec bh
          bhPos    = (holePos bh) ^+^ (0.1 *^ dirPlane)
          dir      = Vec3d(0,0,1)
          rotAxis  =  dir `cross` dirPlane
          rotAngle = acos(dir .* dirPlane) / pi * 180
          p1 = vertex3 (Vec3d(-1, -1, 0))
          p2 = vertex3 (Vec3d(1, -1, 0))
          p3 = vertex3 (Vec3d(1, 1, 0))
          p4 = vertex3 (Vec3d(-1, 1, 0))
      
      blend $= Enabled
      -- SrcAlpha: 源颜色的 Alpha 值， 
      blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
      preservingMatrix $ do
          loadIdentity
          translate $ vector3 $ bhPos
          rotate rotAngle $ vector3 rotAxis
          texture Texture2D $= Enabled
          textureFunction $= Replace
          textureBinding Texture2D $= Just (texBulletHole textures)
          let verts = [p1, p2, p3, p4]
              (tx0, ty0) = (0.0, 0.0)
              (tx1, ty1) = (1.0, 1.0)
              texs = [(TexCoord2 tx0 ty1), (TexCoord2 tx0 ty0), (TexCoord2 tx1 ty0), (TexCoord2 tx1 ty1)]
          renderPrimitive Quads $ do mapVerticies texs verts
          texture Texture2D $= Disabled
      blend $= GL.Disabled



{-
mapVerticies :: [(TexCoord2 GLfloat)] -> [(Vertex3 GLfloat)] -> IO ()
mapVerticies texs verts = mapM_ setVertex (zip texs verts)

setVertex :: (TexCoord2 GLfloat, Vertex3 GLfloat) -> IO ()
setVertex (texCoordinates, vertexCoordinates) = do texCoord texCoordinates; vertex vertexCoordinates;
-}

-- 玩家渲染
renderPlayer :: Player -> IO ()
renderPlayer p = preservingMatrix $ do
    loadIdentity
    translate $ vector3 $ playerPos p
    materialDiffuse Front $= Color4 0 0 0 1
    materialSpecular FrontAndBack $= Color4 0 0 0 1
    materialAmbient FrontAndBack $= Color4 0 0 0 1
--    materialEmission FrontAndBack $= computeColor p
    let r = double (playerRadius p)
        radianToDegrees x = 180 * x / pi
        style = QuadricStyle (Just Smooth) GenerateTextureCoordinates Outside FillStyle

    preservingMatrix $ do
        rotate (radianToDegrees $ fst $ playerView p) (vector3 $ Vec3d(0, 0, -1))
        rotate (radianToDegrees $ snd $ playerView p) (vector3 $ Vec3d(0, -1, 0))
        rotate 90 (vector3 $ Vec3d(0,0,1))
        texture Texture2D $= Enabled
        textureFunction $= Decal
        textureBinding Texture2D $= Just (texPlayerHead textures)
        renderQuadric style $ Sphere r 100 100
        texture Texture2D $= Disabled
        --renderQuad (Just (texPlayer textures)) (Vertex3 (0) r (-r)) (Vertex3 (0) (r) r) (Vertex3 0 (-r) r) (Vertex3 0 (-r) (-r))

    preservingMatrix $ do
        loadIdentity -- 恢复到初始坐标
        translate (vector3 (playerPos p ^-^ Vec3d(0 , 0, 70)))
        texture Texture2D $= Enabled
        textureFunction $= Decal
        textureBinding Texture2D $= Just (texPlayerBody textures)
        renderQuadric style $ Cylinder (r * 1.5 ) (r) 50 100 100
        texture Texture2D $= Disabled

    -- mvm <- get (matrix (Modelview 0))
    -- now find the normal and rotate the image accordingly
    mv <- get (matrix (Just (Modelview 0))) :: IO (GLmatrix GLdouble)
    mvc <- getMatrixComponents RowMajor mv
    let offset =  float (length $ playerName p) / 2.0
        {-newMT = newMatrix RowMajor [ 1 , 0 , 0 , mvc!!3 ,
                                     0 , 1 , 0 , mvc!!7 ,
                                     0 , 0 , 1 , mvc!!11 ,
                                     0 , 0 , 0 , mvc!!15 ]-}

    -- TODO: Fix this billboard-style name tag overhead
    preservingMatrix $ do
        loadIdentity
        --let v = Vec3d(Matrix[8], Matrix[9], -Matrix[10]);

        translate (vector3 (playerPos p ^+^ Vec3d(0 , 7.5 , 20)))
        GL.scale 0.7 0.7 (0.7 :: GLdouble)
        -- 这里先写的后操作
        rotate (-90) (vector3 $ Vec3d(0,0,1))
        rotate 90 (vector3 $ Vec3d(1,0,0))
        -- Undo Rotations
        -- Redo Scalings
        GLFW.renderString GLFW.Fixed8x16 (playerName p)
        -- Use withMatrix... or access elements directly
        --multMatrix newMT


renderSelf :: Player -> IO()
renderSelf p = do
    let (theta,phi) = playerView p
        mkRMatrixT, mkRMatrixP, mkTMatrix :: IO (GLmatrix Float)
        (ct,st,c,s) = (cos theta,sin theta,cos phi,sin phi)
        --u = Vec3d (ct,st,0) `cross` Vec3d (0,0,1)
        --(x,y,z) = (getx u,gety u,getz u)
        -- y轴旋转矩阵 （这里已经考虑转置）
        mkRMatrixP = newMatrix RowMajor [ c , 0 , s , 0 ,
                                          0 , 1 , 0 , 0 ,
                                         -s , 0 , c , 0 ,
                                          0 , 0 , 0 , 1 ]
        -- z轴旋转矩阵
        mkRMatrixT = newMatrix RowMajor [ ct,-st, 0 , 0 ,
                                          st, ct, 0 , 0 ,
                                          0 , 0 , 1 , 0 ,
                                          0 , 0 , 0 , 1 ]
        -- 平移矩阵
        mkTMatrix = newMatrix RowMajor [1,0,0,negate $ getx $ playerPos p,
                                        0,1,0,negate $ gety $ playerPos p, 
                                        0,0,1,negate $ getz $ playerPos p,
                                        0,0,0,1]
     in do
        renderOrtho widthf heightf $ do
            renderText2D 5 0 ("Life: " ++ show (round (playerLife p))) 2
            renderText2D 5 50 ("Pos : " ++ show (playerPos  p)) 1
            renderText2D 5 70 ("Vel : " ++ show (playerVel  p)) 1
            blend $= Enabled
            -- SrcAlpha: 源颜色的 Alpha 值， 
            blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
            textureFunction $= Replace
            


            let w = widthfl / 2
                h = heightfl / 2
            GL.preservingMatrix $ do
                GL.loadIdentity
                GL.alphaFunc $= Just (Greater,0.1:: Float)
                GL.translate (vector3 $ Vec3d(centerCoordX, centerCoordY, 0))
                GL.texture GL.Texture2D $= GL.Enabled
                GL.textureFunction $= Replace
                GL.textureBinding Texture2D $= Just (texGun textures)
                GL.preservingMatrix $ do
                   let verts = [(Vertex3 (-w) (-h) 0), (Vertex3 (-w) h 0), (Vertex3 w h 0), (Vertex3 w (-h) 0)]
                       (tx0, ty0) = (0.0, 0.0)
                       (tx1, ty1) = (1.0, 1.0)
                       texs = [(TexCoord2 tx0 ty0), (TexCoord2 tx0 ty1), (TexCoord2 tx1 ty1), (TexCoord2 tx1 ty0)]
                   GL.renderPrimitive Quads $ do mapVerticies texs verts
                GL.texture Texture2D $= Disabled
            GL.blend $= GL.Disabled

        GL.matrixMode $= Projection
        initFrustum
        rMatrixT <- mkRMatrixT
        rMatrixP <- mkRMatrixP
        tMatrix  <- mkTMatrix
        GL.multMatrix rMatrixP
        GL.multMatrix rMatrixT
        GL.multMatrix tMatrix

        matrixMode $= Modelview 0