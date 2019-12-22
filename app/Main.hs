{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHC.TypeNats                   (KnownNat)
import Math.NumberTheory.Moduli.Class (Mod)
import Utils.ModHelper                (runMod)


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
