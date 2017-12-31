
module GameCore where

import FRP.Yampa
import Data.List

import CollisionTest
import Collection
import Object
import CallBackFunc

-- 使用的函数（Yampa）
{- dpSwitch :: Functor col =>       (col is a collection)
     (forall sf . (a -> col sf -> col (b, sf))) ->  route : 分配函数，将输入分配到合适的信号函数中
     col (SF b c) ->                                        信号函数集合
     SF (a, col c) (Event d) ->                             存储转换事件
     (col (SF b c) -> d -> SF a (col c)) ->                 执行转换事件并回调
     SF a (col c)
-}

{-
notYet :: SF (Event a) (Event a)
notYet = initially NoEvent

initially :: a -> SF a a
-}

gameCore :: IL ObjectSF -> SF (GameInput, IL ObjOutput) (IL ObjOutput)
gameCore initObjs = dpSwitch route
                             initObjs
                             (arr killOrSpawn >>> notYet)
                             (\sfs f -> gameCore (f sfs))

 -- sf: signal function
route :: (GameInput, IL ObjOutput) -> IL sf -> IL (ObjInput, sf)
route (gi,oos) objs = mapIL (route' oos) objs
    where route' oos (k,obj) =
            (ObjInput {oiGameInput = gi,
                       oiColliding = case getIL k oos of
                                     -- 判定当前对象和剩下的对象中是否有碰撞 
                                     -- find :: Foldable t => (a -> Bool) -> t a -> Maybe a
                                     Just oo -> find (collidesWith (ooBounds oo) . ooBounds)
                                                     (map snd $ assocsIL $ deleteIL k oos)
                                     Nothing -> Nothing
                     },
            obj)

killOrSpawn :: (a, IL ObjOutput) -> Event (IL ObjectSF -> IL ObjectSF)
killOrSpawn (_,oos) = Event $ foldl (.) id funcs
    where funcs = map (\(k,oo) -> ((event id (\_ -> deleteIL k)) (ooKillReq oo)) .
                                  ((foldl (.) id . map insertIL) (ooSpawnReq oo)))
                      (assocsIL oos)

