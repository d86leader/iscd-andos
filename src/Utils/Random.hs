module Utils.Random
( mapRandom
) where

import System.Random                   (RandomGen, next)


mapRandom :: RandomGen g => (a -> g -> (b, g)) -> [a] -> g -> ([b], g)
mapRandom fun xs gen =
    let infiniteGens = gen : map (snd . next) infiniteGens
        vals = zipWith fun xs infiniteGens
        gens = zipWith const infiniteGens vals
        lastGen = if null gens
                    then gen
                    else last gens
    in (map fst vals, lastGen)
