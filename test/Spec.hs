{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, property)
import Protocol (chooseModulo, moduloValue, choosePseudoResidue, obfuscatedBit, isResidue)
import System.Random (mkStdGen)
import GHC.TypeNats    (KnownNat)
import Math.NumberTheory.Moduli.Class (Mod)
import ModHelpers (runMod)
import Debug.Trace (trace)

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
    let gen       = mkStdGen seed
        (m, _gen) = chooseModulo gen
        pse       = choosePseudoResidue m
        obfus     = runMod (moduloValue m) $ helper pse
    in if (isResidue m obfus) /= bit
       then property True
       else trace ("counter example: m = " ++ show m ++ "; pse = " ++ show pse ++ "; obfus = " ++ show obfus ++ "; seed = " ++ show seed) $ property False
    where
        helper :: forall m. KnownNat m => Integer -> Mod m
        helper pse =
            let gen           = mkStdGen seed
                (_, gen1)     = chooseModulo gen
                (obfus, _gen) = obfuscatedBit pse bit gen1
            in obfus
