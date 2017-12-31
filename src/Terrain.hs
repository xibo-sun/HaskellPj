{-# LANGUAGE PackageImports #-}
module Terrain where


import Graphics.Rendering.OpenGL
import qualified "GLFW" Graphics.UI.GLFW as GLFW hiding (time)

import Render
import Vec3d
import CollisionTest
import Common


data Transform = Transform{toffset :: Vec3d,
                           tscale :: Vec3d,
                           ttheta :: Float,
                           tphi :: Float}
    deriving Show


data TerrainElement = Quad Vec3d Vec3d Vec3d Vec3d TextureObject Transform
                    | Cube Height TextureObject Transform
                    | GLUQuadric QuadricPrimitive QuadricStyle TextureObject Transform

instance Show TerrainElement where
    show _ = "TerrainElement"


nullTransform :: Transform
nullTransform = Transform{toffset=(Vec3d(0.0,0.0,0.0)), tscale=(Vec3d(1.0,1.0,1.0)), ttheta=0.0, tphi=0.0}

floorTran0 :: Transform
floorTran0 = Transform{toffset=(Vec3d(-30.0,0.0,-48.0)), tscale=(Vec3d(1000.0,500.0,1000.0)), ttheta=0.0, tphi=0.0}

floorTran1 :: Transform
floorTran1 = Transform{toffset=(Vec3d(-30.0,500.0,-48.0)), tscale=(Vec3d(1000.0,1.0,200.0)), ttheta=0.0, tphi=0.0}

floorTran2 :: Transform
floorTran2 = Transform{toffset=(Vec3d(-30.0,-500.0,-48.0)), tscale=(Vec3d(1000.0,1.0,200.0)), ttheta=0.0, tphi=0.0}

floorTran3 :: Transform
floorTran3 = Transform{toffset=(Vec3d(-30.0,-500.0,-48.0)), tscale=(Vec3d(1.0,1000.0,200.0)), ttheta=0.0, tphi=0.0}

floorTran4 :: Transform
floorTran4 = Transform{toffset=(Vec3d(970.0,-500.0,-48.0)), tscale=(Vec3d(1.0,1000.0,200.0)), ttheta=0.0, tphi=0.0}

cubeTran0 :: Transform
cubeTran0 = Transform{toffset=(Vec3d(300.0,170.0,50.0)), tscale=(Vec3d(1.0,1.0,1.0)), ttheta=0.0, tphi=0.0}

cubeTran1 :: Transform
cubeTran1 = Transform{toffset=(Vec3d(300.0,-170.0,50.0)), tscale=(Vec3d(1.0,1.0,1.0)), ttheta=0.0, tphi=0.0}

cubeTran2 :: Transform
cubeTran2 = Transform{toffset=(Vec3d(640.0,170.0,50.0)), tscale=(Vec3d(1.0,1.0,1.0)), ttheta=0.0, tphi=0.0}

cubeTran3 :: Transform
cubeTran3 = Transform{toffset=(Vec3d(640.0,-170.0,50.0)), tscale=(Vec3d(1.0,1.0,1.0)), ttheta=0.0, tphi=0.0}

cubeTranWall0 :: Transform
cubeTranWall0 = Transform{toffset=(Vec3d(210.0,320.0,-20.0)), tscale=(Vec3d(0.2,1.0,0.6)), ttheta=0.0, tphi=0.0}

cubeTranWall1 :: Transform
cubeTranWall1 = Transform{toffset=(Vec3d(210.0,-320.0,-20.0)), tscale=(Vec3d(0.2,1.0,0.6)), ttheta=0.0, tphi=0.0}

cubeTranWall2 :: Transform
cubeTranWall2 = Transform{toffset=(Vec3d(730.0,320.0,-20.0)), tscale=(Vec3d(0.2,1.0,0.6)), ttheta=0.0, tphi=0.0}

cubeTranWall3 :: Transform
cubeTranWall3 = Transform{toffset=(Vec3d(730.0,-320.0,-20.0)), tscale=(Vec3d(0.2,1.0,0.6)), ttheta=0.0, tphi=0.0}

cubeTranWall4 :: Transform
cubeTranWall4 = Transform{toffset=(Vec3d(470.0,450.0,-20.0)), tscale=(Vec3d(0.2,1.0,0.6)), ttheta=0.0, tphi=0.0}

cubeTranWall5 :: Transform
cubeTranWall5 = Transform{toffset=(Vec3d(470.0,-450.0,-20.0)), tscale=(Vec3d(0.2,1.0,0.6)), ttheta=0.0, tphi=0.0}


floorQuad :: [TerrainElement]
floorQuad = [Quad (Vec3d(0,-1,0)) (Vec3d(1,-1,0)) (Vec3d(1,1,0)) (Vec3d(0,1,0)) (texFloor textures) floorTran0,
             Quad (Vec3d(0,0,0)) (Vec3d(1,0,0)) (Vec3d(1,0,1)) (Vec3d(0,0,1)) (texFloor textures) floorTran1,
             Quad (Vec3d(0,0,0)) (Vec3d(1,0,0)) (Vec3d(1,0,1)) (Vec3d(0,0,1)) (texFloor textures) floorTran2,
             Quad (Vec3d(0,0,0)) (Vec3d(0,1,0)) (Vec3d(0,1,1)) (Vec3d(0,0,1)) (texFloor textures) floorTran3,
             Quad (Vec3d(0,0,0)) (Vec3d(0,1,0)) (Vec3d(0,1,1)) (Vec3d(0,0,1)) (texFloor textures) floorTran4]

cubeWalls :: [TerrainElement]
cubeWalls =  [Cube 200 (texWall textures) cubeTran0,
              Cube 200 (texWall textures) cubeTran1,
              Cube 200 (texWall textures) cubeTran2,
              Cube 200 (texWall textures) cubeTran3,
              Cube 100 (texWall2 textures) cubeTranWall0,
              Cube 100 (texWall2 textures) cubeTranWall1,
              Cube 100 (texWall2 textures) cubeTranWall2,
              Cube 100 (texWall2 textures) cubeTranWall3,
              Cube 100 (texWall2 textures) cubeTranWall4,
              Cube 100 (texWall2 textures) cubeTranWall5]

skyQuadric :: [TerrainElement]
skyQuadric = [GLUQuadric (Sphere 1200 1200 1200) (QuadricStyle (Just Smooth) GenerateTextureCoordinates Inside FillStyle) (texSky textures) nullTransform]


defTerrain :: [TerrainElement]
defTerrain = cubeWalls ++ floorQuad ++ skyQuadric


applyTransform :: Transform -> IO()
applyTransform Transform{toffset=offset, tscale=Vec3d(sx,sy,sz), ttheta = thetaAngle, tphi = phiAngle} = do
    translate $ vector3 offset
    Graphics.Rendering.OpenGL.scale sx sy sz
    rotate thetaAngle $ vector3 (Vec3d(0.0, 0.0, 1.0))
    rotate phiAngle $ vector3 (Vec3d(1.0, 0.0, 0.0))


applyTransform2 :: Transform -> Vec3d -> Vec3d
applyTransform2 Transform{toffset=Vec3d(dx,dy,dz), tscale=Vec3d(sx,sy,sz), ttheta = thetaAngle, tphi = phiAngle} (Vec3d (x,y,z)) =
    Vec3d (sx*x + dx, sy*y + dy, sz*z + dz)


renderTerrainElement :: TerrainElement -> IO()
renderTerrainElement (Quad p1 p2 p3 p4 texObj transform) = do
    preservingMatrix $ do
        applyTransform transform
        renderQuad (Just texObj) (vertex3 p1) (vertex3 p2) (vertex3 p3) (vertex3 p4)


renderTerrainElement (Cube height texObj transform) = do
    preservingMatrix $ do
      let h2 = (float height)/2
          p1 = Vertex3 (h2) (h2) (h2)
          p2 = Vertex3 (h2) (h2) (-h2)
          p3 = Vertex3 (h2) (-h2) (-h2)
          p4 = Vertex3 (h2) (-h2) (h2)
          p5 = Vertex3 (-h2) (h2) (h2)
          p6 = Vertex3 (-h2) (h2) (-h2)
          p7 = Vertex3 (-h2) (-h2) (-h2)
          p8 = Vertex3 (-h2) (-h2) (h2)
      
      applyTransform transform
      renderQuad (Just texObj) p1 p2 p3 p4
      renderQuad (Just texObj) p1 p5 p8 p4
      renderQuad (Just texObj) p5 p6 p7 p8
      renderQuad (Just texObj) p6 p7 p3 p2
      renderQuad (Just texObj) p1 p2 p6 p5
      renderQuad (Just texObj) p3 p4 p8 p7

renderTerrainElement (GLUQuadric qprimitive qstyle texObj transform) = do
--  print "Rendering GLUQuadric"
  preservingMatrix $ do
      texture Texture2D $= Enabled
      textureFunction $= Decal
      textureBinding Texture2D $= Just texObj
      applyTransform transform
      renderQuadric qstyle qprimitive
      texture Texture2D $= Disabled

getTerrainBounds :: [TerrainElement] -> BoundingVolume
getTerrainBounds telements = MultipleVolumes $ map getTerrainBound telements


getTerrainBound :: TerrainElement -> BoundingVolume
getTerrainBound (Cube height _ trans) =
    BoundingBox (applyTransform2 trans (Vec3d (-h2,-h2,-h2))) (applyTransform2 trans (Vec3d (h2,h2,h2)))
        where h2 = float height/2

getTerrainBound (GLUQuadric (Cylinder innerRad outerRad height _ _) _ _ trans) =
    BoundingBox (applyTransform2 trans (Vec3d (-maxR,-maxR,0.0))) (applyTransform2 trans (Vec3d (maxR,maxR,h)))
        where h = float height
              maxR = max (float outerRad) (float innerRad)

getTerrainBound (Quad p1 p2 p3 p4 _ trans) =
        BoundingBox (Vec3d (minx,miny,minz)) (Vec3d (maxx,maxy,maxz))
            where ps = map (applyTransform2 trans) [p1,p2,p3,p4]
                  minx = minimum $ map getx ps
                  miny = minimum $ map gety ps
                  minz = minimum $ map getz ps
                  maxx = maximum $ map getx ps
                  maxy = maximum $ map gety ps
                  maxz = maximum $ map getz ps

getTerrainBound _ = BoundingEmpty

