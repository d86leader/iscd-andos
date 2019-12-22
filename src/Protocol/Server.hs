{-# LANGUAGE ScopedTypeVariables #-}
module Protocol.Server
( chooseModulo
, Modulo (..), moduloValue
, choosePseudoResidue
, isResidue
, isPseudoResidue
, obfuscatedBit
, obfuscatedMessages
) where

import Math.NumberTheory.Euclidean     (coprime)
import Math.NumberTheory.Moduli.Jacobi (JacobiSymbol (MinusOne, One), jacobi)
import Math.NumberTheory.Primes        (Prime, isPrime, unPrime)
import System.Random                   (RandomGen, next, randomR)

import Secrets (SecretStore, bitsOfAll)


data Modulo = Modulo Integer (Prime Integer) (Prime Integer)
    deriving (Show)
moduloValue :: Modulo -> Integer
moduloValue (Modulo v _ _) = v

choosePrime :: RandomGen g => g -> (Prime Integer, g)
choosePrime gen =
    let (num, gen') = randomR (3, high) gen
    in case isPrime num of
       Just prime -> (prime, gen')
       _          -> choosePrime gen'
    where high = 2 ^ 16 - 1

chooseModulo :: RandomGen g => g -> (Modulo, g)
chooseModulo gen =
    let (p, gen')  = choosePrime gen
        (q, gen'') = choosePrime gen'
    in  (Modulo (unPrime p * unPrime q) p q, gen'')


jacobiOne :: JacobiSymbol -> Bool
jacobiOne One = True
jacobiOne _   = False
jacobiMinus :: JacobiSymbol -> Bool
jacobiMinus MinusOne = True
jacobiMinus _        = False


isResidue :: Modulo -> Integer -> Bool
isResidue (Modulo _ p' q') num =
    let p = unPrime p'
        q = unPrime q'
    in jacobiOne (jacobi num p) && jacobiOne (jacobi num q)

isNonResidue :: Modulo -> Integer -> Bool
isNonResidue (Modulo _ p' q') num =
    let p = unPrime p'
        q = unPrime q'
    in jacobiMinus (jacobi num p) && jacobiMinus (jacobi num q)

isPseudoResidue :: Modulo -> Integer -> Bool
isPseudoResidue modulo@(Modulo m _ _) num =
    jacobiOne (jacobi num m) && isNonResidue modulo num

choosePseudoResidue :: Modulo -> Integer
choosePseudoResidue modulo = choosePseudoResidue' 2 where
    choosePseudoResidue' num =
        if  isPseudoResidue modulo num
        then num
        else if num >= moduloValue modulo
        then error "Couldn't find pseudo residue"
        else choosePseudoResidue' (num + 1)


-- | Residue when False, not residue when True
obfuscatedBit :: forall g. RandomGen g
              => Modulo -> Integer -> Bool -> g -> (Integer, g)
obfuscatedBit modulo pseudResd b gen =
    let mval = moduloValue modulo
        (x, gen') = genCoprime mval $ gen
        z = x * x * if b then pseudResd
                         else 1
    in (z, gen')
    where
        genCoprime :: Integer -> g -> (Integer, g)
        genCoprime mval g =
            let (x, g') = randomR (2, mval) g
            in if x `coprime` mval
               then (x, g')
               else genCoprime mval g'


-- | Generate a number for each bit that is residue when 0 and not residue when 1
obfuscatedMessages :: RandomGen g
                   => SecretStore -> Modulo -> Integer -> g
                   -> ([[Integer]], g)
obfuscatedMessages store modulo pseudResd gen =
    let bits = bitsOfAll store
    in mapRandom (mapRandom $ obfuscatedBit modulo pseudResd) bits gen

mapRandom :: RandomGen g => (a -> g -> (b, g)) -> [a] -> g -> ([b], g)
mapRandom fun xs gen =
    let gens = gen : map (snd . next) gens
        vals = zipWith fun xs gens
    in (map fst vals, last gens)
