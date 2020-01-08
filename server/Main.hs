{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase     #-}
module Main where

import Api                      (AndosApi, Token (Token), andosApi)
import Control.Concurrent.MVar  (MVar, modifyMVar, newMVar, readMVar)
import Control.Monad            (when)
import Control.Monad.IO.Class   (liftIO)
import Data.Map.Strict          (Map, empty, lookup, insert)
import Data.Text.Encoding       (decodeUtf8)
import Data.Vector              (toList)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Servant.API              ((:<|>) ((:<|>)))
import Servant.Server           (Server, serve)
import System.Random            (getStdGen, setStdGen, random)
import Protocol.Server
    (Modulo, chooseModulo, choosePseudoResidue, obfuscatedMessage, isResidue)
import Secrets
    (SecretStore, StringIndex (..), readStore, storeDescriptions, storeRowBits)

import Prelude hiding (lookup)


data Cookie = Cookie
    { modulo :: Modulo
    , pse    :: Integer
    , qsLeft :: Int
    }
type Database = MVar (Map Int Cookie) -- how many requests are left for a token

andosServer :: SecretStore -> Database -> Bool -> Server AndosApi
andosServer store database verbose =
    listDescriptions :<|> buy :<|> getRow :<|> verify where
    --
    listDescriptions =
        pure . map decodeUtf8 . toList . storeDescriptions $ store
    --
    buy = liftIO . modifyMVar database $ \db -> do
        gen0 <- getStdGen
        let (token, gen1)  = random gen0
        let (modulo, gen2) = chooseModulo gen1
        let pse            = choosePseudoResidue modulo
        let qsLeft         = storeRowBits store
        let cookie = Cookie {modulo, pse, qsLeft}
        setStdGen gen2
        --
        when verbose $
            putStrLn $ "For client " ++ show token
                    ++ " generated modulo " ++ show modulo
                    ++ " and pseudo-residue " ++ show pse
        --
        let db' = insert token cookie db
        return (db', Token token)
    --
    getRow (Token token) index = liftIO $
        lookup token <$> readMVar database >>= \case
          Nothing -> pure []
          Just Cookie {modulo, pse} -> do
            gen0 <- getStdGen
            let (obf, gen1) = obfuscatedMessage store (StringIndex index) modulo pse gen0
            setStdGen gen1
            --
            when verbose $
                putStrLn $ "For client " ++ show token
                        ++ " giving row number " ++ show index
                        ++ " as values: " ++ show obf
            --
            return obf
    --
    verify (Token token) query = liftIO . modifyMVar database $ \db ->
        case lookup token db of
          Nothing -> pure (db, Nothing)
          Just cookie @ Cookie {modulo, qsLeft} -> do
            let response = Just $ isResidue modulo query
            let db' = insert token cookie {qsLeft = qsLeft - 1} db
            --
            when verbose $
                putStrLn $ "For client " ++ show token
                        ++ " on query " ++ show query
                        ++ " giving response " ++ show response
                        ++ "; questions left: " ++ show (qsLeft - 1)
            --
            return (db', response)


main :: IO ()
main = do
    store <- readStore "secrets.yaml"
    db <- newMVar empty
    let app = serve andosApi $ andosServer store db True
    --
    putStrLn $ "Serving on " ++ show port
    runSettings settings app
  where
    settings = setPort port
             $ defaultSettings
    port = 1337
