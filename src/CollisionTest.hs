module CollisionTest where
import FRP.Yampa
import qualified Graphics.Rendering.OpenGL as GL
import Vec3d
import Common

data BoundingVolume = BoundingBox Vec3d Vec3d 
                    | BoundingEmpty           
                    | MultipleVolumes [BoundingVolume]
    deriving (Show, Eq)

collidesWith :: BoundingVolume -> BoundingVolume -> Bool
collidesWith BoundingEmpty _ = False
collidesWith _ BoundingEmpty = False
collidesWith (BoundingBox (Vec3d (minx1,miny1,minz1)) (Vec3d (maxx1,maxy1,maxz1)))
             (BoundingBox (Vec3d (minx2,miny2,minz2)) (Vec3d (maxx2,maxy2,maxz2))) =
                   minx1 < maxx2 && maxx1 > minx2 &&
                   miny1 < maxy2 && maxy1 > miny2 &&
                   minz1 < maxz2 && maxz1 > minz2
collidesWith (MultipleVolumes vols) bv = or $ map (collidesWith bv) vols
collidesWith bv (MultipleVolumes vols) = or $ map (collidesWith bv) vols



collisionLP :: (Bullet, Bullet, Player) -> Maybe Hit
collisionLP (lprev, l, p) =
    let p1 = bulletPos lprev
        p2 = bulletPos l
        p3 = playerPos p
        seg = p2 ^-^ p1
        u = ((p3 ^-^ p1) .* seg) / (seg .* seg)
        ipt = p1 ^+^ (u *^ seg)
    in if 0 <= u && u <= 1 && len (p3 ^-^ ipt) < playerRadius p
       then Just $ Hit{player1ID=bulletpID l, player2ID=playerID p, hitBulletID=bulletID l}
       else Nothing


collisionPoint :: (Bullet, Bullet, Plane) -> Maybe BulletHole
collisionPoint (lprev, l, p) = let p1              = bulletPos lprev
                                   p2              = bulletPos l
                                   Vec3d (vp1, vp2, vp3) = planeVec p
                                   Vec3d (n1, n2 ,n3)    = planePoint p
                                   Vec3d (v1, v2, v3)    = (p2 ^-^ p1)
                                   Vec3d (m1, m2, m3)    = p1
                                   vpt = v1 * vp1 + v2 * vp2 + v3 * vp3
                                in case vpt of 
                                        0.0 -> Nothing
                                        _   -> let t  = ((n1 - m1) * vp1 + (n2 - m2) * vp2 + (n3 - m3) * vp3) / vpt
                                                   r1 = (m1 + v1 * t)
                                                   r2 = (m2 + v2 * t) 
                                                   r3 = (m3 + v3 * t)
                                                   l1 = Vec3d(r1,r2,r3) ^-^ p1
                                                   l2 = (p2 ^-^ p1)
                                                   (minX, maxX, minY, maxY, minZ, maxZ) = planeRange p
                                                   bh = BulletHole { holepID = bulletpID l,
                                                                     holePos = Vec3d(r1, r2, r3),
                                                                     holeVec = planeVec p}
                                                   isHit =  r1 >= minX && r1 <= maxX &&
                                                            r2 >= minY && r2 <= maxY &&
                                                            r3 >= minZ && r3 <= maxY && 
                                                            len l2 >= len l1
                                                in case isHit of 
                                                        True -> Just bh
                                                        _  -> Nothing