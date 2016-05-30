{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad (replicateM)
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield atts defs) = do
    let aDice = atts - (max 1 (atts - 3))
        dDice = defs - (max 0 (defs - 2))
    aRolls <- fmap (map unDV) $ replicateM aDice die
    dRolls <- fmap (map unDV) $ replicateM dDice die
    let rolls = zip (reverse $ sort aRolls)
                    (reverse $ sort dRolls)
        losses (a,d) (a',d') = if a > d then (a'+1,d') else (a',d'+1)
        (aLoss, dLoss) = foldr losses (0,0) rolls
    return (Battlefield (atts - aLoss) (defs - dLoss))

