-- v <- integral -< a
-- integral :: VectorSpace a s => SF a a
-- (^<<) :: Arrow a => (c -> d) -> a b c -> a b d
-- tag :: Event a -> b -> Event b
-- event :: a -> (b -> a) -> Event b -> a
-- repeatedly :: Time -> b -> SF a (Event b)

-- mergeBy :: (a -> a -> a) -> Event a -> Event a -> Event a
-- Applicative-based definition: mergeBy f re le = (f $ re * le) | re | le