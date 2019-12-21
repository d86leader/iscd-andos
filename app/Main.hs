{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Main where

import Data.Reflection (reifyNat)
import GHC.TypeNats    (KnownNat)
import Math.NumberTheory.Moduli.Class (Mod, (^%), getVal)
import ModHelpers (runMod)


add :: KnownNat m => Mod m -> Mod m -> Mod m
add = (+)

main :: IO ()
main = do
    putStrLn "input modulo"
    n :: Integer <- fmap read getLine
    putStrLn "input first value"
    a :: Integer <- fmap read getLine
    putStrLn "input second value"
    b :: Integer <- fmap read getLine
    let r = runMod n (add (fromInteger a) (fromInteger b))
    print r
