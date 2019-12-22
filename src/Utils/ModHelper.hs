{-# LANGUAGE Rank2Types #-}
module Utils.ModHelper
( runMod
) where

import Data.Proxy                     (Proxy)
import Data.Reflection                (reifyNat)
import GHC.TypeNats                   (KnownNat)
import Math.NumberTheory.Moduli.Class (Mod, getVal)


runMod :: Integer -> (forall m. KnownNat m => Mod m) -> Integer
runMod m val = reifyNat m $ extractMod val

extractMod :: KnownNat m => Mod m -> Proxy m -> Integer
extractMod val _proxy = getVal val
