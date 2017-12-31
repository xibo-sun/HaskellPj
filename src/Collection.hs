{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Collection where

import FRP.Yampa

type ILKey = Int

data IL a = IL {
    ilNextKey :: ILKey,
    ilAssocs :: [(ILKey, a)]
}   deriving (Show, Eq)

instance Functor IL where
    fmap f (IL {ilNextKey = nk, ilAssocs = assocs}) =
        IL {ilNextKey = nk, ilAssocs = map (\(i,a) -> (i,f a)) assocs}

instance VectorSpace s a => VectorSpace (IL s) a where
    zeroVector = emptyIL
    (*^) k = fmap (k *^)
    (^/) vs k = fmap (^/ k) vs
    negateVector = fmap negateVector
    (^+^) = zipWithIL (^+^) (const Nothing) Just    -- This is a total hack to make it work with Yampa's integral definition!
    (^-^) = error "(^-^) called on VectorSpace (IL s) a"
    dot _ _ = error "dot called on VectorSpace (IL s) a"
    norm _ = error "norm called on VectorSpace (IL s) a"
    normalize _ = error "normalize called on VectorSpace (IL s) a"

emptyIL :: IL a
emptyIL = IL {ilNextKey = 0, ilAssocs = []}

insertIL :: a -> IL a -> IL a
insertIL a (IL {ilNextKey = nk, ilAssocs = assocs}) = IL {ilNextKey = nk+1, ilAssocs = (nk,a):assocs}

listToIL :: [a] -> IL a
listToIL as = foldl (\il a -> insertIL a il) emptyIL as

elemsIL :: IL a -> [a]
elemsIL = reverse . map snd . ilAssocs

assocsIL :: IL a -> [(ILKey,a)]
assocsIL = ilAssocs

getIL :: ILKey -> IL a -> Maybe a
getIL k il = getIL' k $ ilAssocs il
    where getIL' k ((k',v):rest) = if k == k' then Just v else getIL' k rest
          getIL' k [] = Nothing

deleteIL :: ILKey -> IL a -> IL a
deleteIL k il = il {ilAssocs = deleteIL' k (ilAssocs il)}
    where deleteIL' k ((k',v):rest) = if k == k' then rest else (k',v) : deleteIL' k rest
          deleteIL' k [] = error "Attempted to delete a non-existent element from an IL."

mapIL :: ((ILKey,a) -> b) -> IL a -> IL b
mapIL f il = il {ilAssocs = map (\(k,a) -> (k, f (k,a))) $ ilAssocs il}

filterIL :: (a -> Bool) -> IL a -> IL a
filterIL f il = il {ilAssocs = filter (\(_,a) -> f a) $ ilAssocs il}

zipWithIL :: (a -> b -> c) -> (a -> Maybe c) -> (b -> Maybe c) -> IL a -> IL b -> IL c
zipWithIL f fa fb ila ilb = let assocs = zipWithIL' f fa fb (ilAssocs ila) (ilAssocs ilb)
                            in IL {ilNextKey = length assocs, ilAssocs = assocs}
    where zipWithIL' f fa fb la@((ka,a):as) lb@((kb,b):bs) =
                case compare ka kb of
                    EQ -> (ka, f a b) : (zipWithIL' f fa fb as bs)
                    GT -> case fa a of
                              Just c -> (ka, c) : (zipWithIL' f fa fb as lb)
                              Nothing -> zipWithIL' f fa fb as lb
                    LT -> case fb b of
                              Just c -> (kb, c) : (zipWithIL' f fa fb la bs)
                              Nothing -> zipWithIL' f fa fb la bs
          zipWithIL' f fa fb ((ka,a):as) [] = case fa a of
                                                  Just c -> (ka,c) : (zipWithIL' f fa fb as [])
                                                  Nothing -> zipWithIL' f fa fb as []
          zipWithIL' f fa fb [] ((kb,b):bs) = case fb b of
                                                  Just c -> (kb,c) : (zipWithIL' f fa fb [] bs)
                                                  Nothing -> zipWithIL' f fa fb [] bs
          zipWithIL' f fa fb [] [] = []

updateIL :: ILKey -> a -> IL a -> IL a
updateIL k v il = insertIL v $ deleteIL k il

