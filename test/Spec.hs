{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Debug.Trace           (trace)
import System.Random         (mkStdGen)
import Test.Tasty            (defaultMain, testGroup)
import Test.Tasty.QuickCheck (Property, property, testProperty)

import Protocol.Server
    (chooseModulo, choosePseudoResidue, isResidue, obfuscatedBit)

main :: IO ()
main = defaultMain . testGroup "All tests" $
    [ testProperty "Good pseudo residue" $ testSelectsPseudoResidue
    , testProperty "Correct bit-residue" $ testResidue
    ]

testSelectsPseudoResidue :: Int -> Property
testSelectsPseudoResidue seed = property $
    let gen = mkStdGen seed
        (m, _) = chooseModulo gen
        pse = choosePseudoResidue m
    in not $ isResidue m pse

testResidue :: Int -> Bool -> Property
testResidue seed bit =
    let gen         = mkStdGen seed
        (m, gen')   = chooseModulo gen
        pse         = choosePseudoResidue m
        (obf, _gen) = obfuscatedBit m pse bit gen'
    in if (isResidue m obf) /= bit
       then property True
       else trace ("counter example: m = " ++ show m ++ "; pse = " ++ show pse
                ++ "; obfus = " ++ show obf ++ "; seed = " ++ show seed)
          $ property False
