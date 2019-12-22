{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}
module Secrets
( SecretStore, createStore, createStore', readStore, StoreIndex
, StringIndex (StringIndex), ElemIndex (ElemIndex)
, getBit, bitsOf, bitsOfAll
) where

import Data.Bifunctor     (bimap)
import Data.Bits          (finiteBitSize, testBit)
import Data.ByteString    (ByteString, index, length, replicate, unpack)
import Data.Monoid        ((<>))
import Data.Text          (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Tuple         (swap)
import Data.Vector        (Vector, fromList, toList, (!))
import Data.Word          (Word8)
import Data.Yaml          (FromJSON (parseJSON), decodeFileThrow)
import Prelude            hiding (length, replicate)


newtype SecretStore = SecretStore {unStore :: Vector ByteString}

createStore :: [ByteString] -> SecretStore
createStore bss =
    let padTo  = maximum . map length $ bss
    in SecretStore . fromList . map (padRight 0 padTo) $ bss
  where
    padRight :: Word8 -> Int -> ByteString -> ByteString
    padRight c n bs =
        let amount = length bs - n
        in bs <> replicate amount c

createStore' :: [Text] -> SecretStore
createStore' = createStore . map encodeUtf8


instance FromJSON SecretStore where
    parseJSON = fmap createStore' . parseJSON

readStore :: FilePath -> IO SecretStore
readStore = decodeFileThrow


newtype StringIndex = StringIndex Int
newtype ElemIndex   = ElemIndex Int
class StoreIndex a where
    canonical :: a -> (StringIndex, ElemIndex)
instance StoreIndex (StringIndex, ElemIndex) where
    canonical = id
instance StoreIndex (ElemIndex, StringIndex) where
    canonical = swap
instance StoreIndex (Int, Int) where
    canonical = bimap StringIndex ElemIndex

getBit :: StoreIndex a => SecretStore -> a -> Bool
getBit (SecretStore store) (canonical -> (StringIndex si, ElemIndex ej)) =
    let (wordIndex, bitIndex) = ej `divMod` 8
    in flip testBit bitIndex . flip index wordIndex . flip (!) si $ store

toBits :: Word8 -> [Bool]
toBits word =
    let size = finiteBitSize word
        inds = [0..size - 1]
    in map (testBit word) inds

bitsOf :: SecretStore -> StringIndex -> [Bool]
bitsOf (SecretStore store) (StringIndex i) =
    concat . map toBits . unpack $ store ! i


bitsOfAll :: SecretStore -> [[Bool]]
bitsOfAll = map (concat . map toBits . unpack) . toList . unStore
