{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Monoid (Monoid(..))

newtype Score = Score Int deriving (Eq, Ord, Show, Num)

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)

