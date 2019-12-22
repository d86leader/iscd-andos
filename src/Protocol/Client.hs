{-# LANGUAGE ScopedTypeVariables #-}
module Protocol.Client
( queryBit
) where


import GHC.TypeNats                   (KnownNat)
import Math.NumberTheory.Euclidean    (coprime)
import Math.NumberTheory.Moduli.Class (Mod, getMod)
-- import Secrets                        (StoreIndex)
import System.Random                  (RandomGen, randomR)


genCoprime :: (RandomGen g, KnownNat m) => g -> (Mod m, g)
genCoprime gen =
    let (x, gen') = randomR (2, mval) $ gen
        x'        = fromInteger x
        mval      = getMod x'
    in if x `coprime` mval
       then (x', gen')
       else genCoprime gen'


queryBit :: (RandomGen g, KnownNat m)
         => Mod m -> Bool -> Mod m -> g -> (Mod m, g)
queryBit pseudoResd bit obfus gen =
    let (x, gen') = genCoprime gen
        z = x * x * obfus * if bit then pseudoResd
                                   else 1
    in (z, gen')
