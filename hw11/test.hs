import Control.Applicative hiding ((*>), mapA, sequenceA, replicateA)

(*>) :: Applicative f => f a -> f b -> f b
l *> r = (id <$ l) <*> r

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = sequenceA . fmap f

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n = fmap (replicate n)

