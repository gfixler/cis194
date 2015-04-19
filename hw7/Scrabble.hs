{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Monoid (Monoid(..))
import Data.Char (toLower)

newtype Score = Score Int deriving (Eq, Ord, Show, Num)

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)

score :: Char -> Score
score x = case lookup (toLower x) values of
              Just v  -> Score v
              Nothing -> Score 0
    where values = zip ['a'..'z'] [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

scoreString :: String -> Score
scoreString = mconcat . map score

