module Utils.Random
( mapRandom
) where

import System.Random                   (RandomGen, next)


mapRandom :: RandomGen g => (a -> g -> (b, g)) -> [a] -> g -> ([b], g)
mapRandom fun xs gen =
    let gens = gen : map (snd . next) gens
        vals = zipWith fun xs gens
    in (map fst vals, last gens)
