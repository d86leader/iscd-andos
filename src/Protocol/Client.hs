{-# LANGUAGE ScopedTypeVariables #-}
module Protocol.Client
( queryBit
, queryMessage
) where


import Math.NumberTheory.Euclidean    (coprime)
import Utils.Random                   (mapRandom)
import System.Random                  (RandomGen, randomR, random)


genCoprime :: RandomGen g => Integer -> g -> (Integer, g)
genCoprime m gen =
    let (x, gen') = randomR (2, m - 1) $ gen
    in if x `coprime` m
       then (x, gen')
       else genCoprime m gen'


-- | When response is True, bit is correctly guessed
queryBit :: RandomGen g
         => Integer -> Integer -> Bool -> Integer -> g -> (Integer, g)
queryBit modulo pseudoResd bit obfus gen =
    let (x, gen') = genCoprime modulo gen
        z = x * x * obfus * if bit then pseudoResd
                                   else 1
    in (z, gen')


queryMessage :: RandomGen g
             => Integer -> [Integer] -> Integer -> g -> ([Bool], [Integer], g)
queryMessage modulo obfs pse gen =
    let (bits, gen1)    = mapRandom (\_ g -> random g) obfs gen
        (queries, gen2) = mapRandom (uncurry (queryBit modulo pse))
                                    (zip bits obfs)
                                    gen1
    in (bits, queries, gen2)
