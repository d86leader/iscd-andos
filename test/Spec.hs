{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Debug.Trace                    (trace)
import GHC.TypeNats                   (KnownNat)
import Math.NumberTheory.Moduli.Class (Mod)
import ModHelpers                     (runMod)
import Protocol
    (chooseModulo, choosePseudoResidue, isResidue, moduloValue, obfuscatedBit)
import System.Random                  (mkStdGen)
import Test.Tasty                     (defaultMain, testGroup)
import Test.Tasty.QuickCheck          (Property, property, testProperty)

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
       else trace ("counter example: m = " ++ show m ++ "; pse = " ++ show pse
                ++ "; obfus = " ++ show obfus ++ "; seed = " ++ show seed)
          $ property False
    where
        helper :: forall m. KnownNat m => Integer -> Mod m
        helper pse =
            let gen           = mkStdGen seed
                (_, gen1)     = chooseModulo gen
                (obfus, _gen) = obfuscatedBit pse bit gen1
            in obfus
