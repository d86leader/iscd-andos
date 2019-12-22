{-# LANGUAGE ScopedTypeVariables #-}
module Protocol.Client
( queryBit
, queryMessage
) where


import GHC.TypeNats                   (KnownNat)
import Math.NumberTheory.Euclidean    (coprime)
import Math.NumberTheory.Moduli.Class (Mod, getMod)
import Utils.Random                   (mapRandom)
import System.Random                  (RandomGen, randomR, random)


genCoprime :: (RandomGen g, KnownNat m) => g -> (Mod m, g)
genCoprime gen =
    let (x, gen') = randomR (2, mval) $ gen
        x'        = fromInteger x
        mval      = getMod x'
    in if x `coprime` mval
       then (x', gen')
       else genCoprime gen'


-- | When response is True, bit is correctly guessed
queryBit :: (RandomGen g, KnownNat m)
         => Mod m -> Bool -> Mod m -> g -> (Mod m, g)
queryBit pseudoResd bit obfus gen =
    let (x, gen') = genCoprime gen
        z = x * x * obfus * if bit then pseudoResd
                                   else 1
    in (z, gen')


queryMessage :: (RandomGen g, KnownNat m)
             => [Mod m] -> Mod m -> g -> ([Bool], [Mod m], g)
queryMessage obfs pse gen =
    let (bits, gen1)    = mapRandom (\_ g -> random g) obfs gen
        (queries, gen2) = mapRandom (uncurry (queryBit pse))
                                    (zip bits obfs)
                                    gen1
    in (bits, queries, gen2)
