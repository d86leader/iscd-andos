{-# LANGUAGE Rank2Types #-}
module ModHelpers
( runMod
) where

import Data.Reflection (reifyNat)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeNats    (KnownNat)
import GHC.Natural     (naturalToInteger)
import Math.NumberTheory.Moduli.Class (Mod, (^%), getVal)


runMod :: Integer -> (forall m. KnownNat m => Mod m) -> Integer
runMod m val = reifyNat m $ extractMod val
  where
    extractMod :: KnownNat m => Mod m -> Proxy m -> Integer
    extractMod val _proxy = getVal val
