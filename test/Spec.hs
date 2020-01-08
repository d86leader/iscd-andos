{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.ByteString           (ByteString, take, empty)
import Debug.Trace               (trace)
import System.Random             (mkStdGen, random)
import Test.QuickCheck.Instances ()
import Test.Tasty                (defaultMain, testGroup)
import Test.Tasty.QuickCheck     (Property, property, testProperty)

import Protocol.Client (queryBit, queryMessage)
import Protocol.Server
    ( chooseModulo, choosePseudoResidue, isResidue, moduloValue
    , obfuscatedBit, obfuscatedMessage
    )
import Secrets         (assembleBits, toBits, createStore, StringIndex (..))

import Prelude hiding (take)

main :: IO ()
main = defaultMain . testGroup "All tests" $
    [ testProperty "Good pseudo residue"          $ testSelectsPseudoResidue
    , testProperty "Correct bit-residue"          $ testResidue
    , testProperty "Meaningful query"             $ testQuery
    , testProperty "Disassembling and assembling" $ testAssembling
    , testProperty "Querying message"             $ testQueryMessage
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
        query = fst $ queryBit (moduloValue m)
                               (fromInteger pse)
                               bitClient
                               (fromInteger obf)
                               gen2
        --
        response = isResidue m query
    in property $ response == (bitServer == bitClient)

testQueryMessage :: Int -> ByteString -> Property
testQueryMessage seed str =
    let gen         = mkStdGen seed
        store       = createStore [str]
        (m, gen1)   = chooseModulo gen
        pse         = choosePseudoResidue m
        (obf, gen2) = obfuscatedMessage store (StringIndex 0) m pse gen1
        --
        (guessed, qs, _) = queryMessage (moduloValue m)
                                        obf pse gen2
        responses = map (isResidue m) qs
        bits = zipWith (==) responses guessed
        result = assembleBits bits
    in property $ result == str

testAssembling :: ByteString -> Property
testAssembling str = property $ str == (assembleBits . toBits) str
