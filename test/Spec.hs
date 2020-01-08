{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.ByteString           (ByteString)
import Debug.Trace               (trace)
import System.Random             (mkStdGen, random)
import Test.QuickCheck.Instances ()
import Test.Tasty                (defaultMain, testGroup)
import Test.Tasty.QuickCheck     (Property, property, testProperty)
import Utils.ModHelper           (runMod)

import Protocol.Client (queryBit)
import Protocol.Server
    (chooseModulo, choosePseudoResidue, isResidue, moduloValue, obfuscatedBit)
import Secrets         (assembleBits, toBits)

main :: IO ()
main = defaultMain . testGroup "All tests" $
    [ testProperty "Good pseudo residue"          $ testSelectsPseudoResidue
    , testProperty "Correct bit-residue"          $ testResidue
    , testProperty "Meaningful query"             $ testQuery
    , testProperty "Disassembling and assembling" $ testAssembling
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


testQuery :: Int -> Bool -> Bool -> Property
testQuery seed bitServer bitClient =
    let gen         = mkStdGen seed
        (m, gen1)   = chooseModulo gen
        pse         = choosePseudoResidue m
        (obf, gen2) = obfuscatedBit m pse bitServer gen1
        --
        query = runMod (moduloValue m)
                       ( fst $ queryBit (fromInteger pse)
                                        bitClient
                                        (fromInteger obf)
                                        gen2
                       )
        --
        response = isResidue m query
    in property $ response == (bitServer == bitClient)

testAssembling :: ByteString -> Property
testAssembling str = property $ str == (assembleBits . toBits) str
