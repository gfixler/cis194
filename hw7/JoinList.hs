{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid
import Data.Maybe
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single x _) = x
tag (Append x _ _) = x

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ y = Append (tag x <> tag y) x y

-- Exercise 2, part 1

instance Sized m => Sized (JoinList m a) where
    size Empty = Size 0
    size (Single s _) = size s
    size (Append s _ _) = size s

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ x) = Just x
indexJ i (Append m x y) = if (i < n) then indexJ i x
                                     else indexJ (i - n) y
    where n = getSize $ size $ tag x
indexJ _ _ = Nothing

