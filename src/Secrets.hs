{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}
module Secrets
( SecretStore, storeSecrets, storeDescriptions, storeRowBits
, createStore, createStore', readStore, StoreIndex
, StringIndex (StringIndex), ElemIndex (ElemIndex)
, getBit, bitsOf
, toBits, assembleBits
) where

import Data.Bifunctor       (bimap)
import Data.Bits            (finiteBitSize, setBit, shiftL, testBit)
import Data.ByteString      (ByteString, index, pack, unpack)
import Data.List.Split      (chunksOf)
import Data.Monoid          ((<>))
import Data.MonoTraversable (olength)
import Data.Text            (Text)
import Data.Text.Encoding   (encodeUtf8)
import Data.Tuple           (swap)
import Data.Vector          (Vector, fromList, (!))
import Data.Word            (Word8)
import Data.Yaml            (FromJSON (parseJSON), decodeFileThrow)

import qualified Data.ByteString as BS


data SecretStore = SecretStore
    { storeSecrets :: Vector ByteString
    , storeDescriptions :: Vector ByteString
    , storeRowBits :: Int
    }

createStore :: [(ByteString, ByteString)] -> SecretStore
createStore pairs =
    let padTo  = maximum . map olength . map fst $ pairs
        secrets = fromList . map (padRight 0 padTo) . map fst $ pairs
        descriptions = fromList . map snd $ pairs
    in SecretStore secrets descriptions padTo
  where
    padRight :: Word8 -> Int -> ByteString -> ByteString
    padRight c n bs =
        let amount = olength bs - n
        in bs <> BS.replicate amount c

createStore' :: [(Text, Text)] -> SecretStore
createStore' = createStore . map (bimap encodeUtf8 encodeUtf8)


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
getBit (storeSecrets -> store) (canonical -> (StringIndex si, ElemIndex ej)) =
    let (wordIndex, bitIndex) = ej `divMod` 8
    in flip testBit bitIndex . flip index wordIndex . flip (!) si $ store

toBitsWord :: Word8 -> [Bool]
toBitsWord word =
    let size = finiteBitSize word
        inds = [0..size - 1]
    in reverse . map (testBit word) $ inds

toBits :: ByteString -> [Bool]
toBits = concat . map toBitsWord . unpack

bitsOf :: SecretStore -> StringIndex -> [Bool]
bitsOf (storeSecrets -> store) (StringIndex i) =
    toBits $ store ! i


assembleBits :: [Bool] -> ByteString
assembleBits bits =
    let prefix = replicate (olength bits `mod` 8) False
    in pack . map assembleWord . chunksOf 8 $ prefix <> bits
  where
    assembleWord :: [Bool] -> Word8
    assembleWord = assembleOnto 0
    assembleOnto x [] = x
    assembleOnto x (True:rest) =
        let x' = shiftL x 1 `setBit` 0
        in assembleOnto x' rest
    assembleOnto x (False:rest) =
        let x' = shiftL x 1
        in assembleOnto x' rest
