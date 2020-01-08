{-# LANGUAGE LambdaCase #-}
module Main where

import Api                    (Token, andosApi)
import Control.Monad.IO.Class (liftIO)
import Data.Text              (Text)
import Network.HTTP.Client    (defaultManagerSettings, newManager)
import Protocol.Client        (queryMessage)
import Secrets                (assembleBits)
import Servant.API            ((:<|>) ((:<|>)))
import Servant.Client
    (ClientM, client, mkClientEnv, parseBaseUrl, runClientM)
import System.Random          (getStdGen, setStdGen)

import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Char8 as BS


listDescriptions :: ClientM [Text]
buy              :: ClientM (Token, Integer, Integer)
getRow           :: Token -> Int -> ClientM [Integer]
verify           :: Token -> Integer -> ClientM (Maybe Bool)
listDescriptions :<|> buy :<|> getRow :<|> verify = client andosApi


main :: IO ()
main = do
    manager   <- newManager defaultManagerSettings
    baseUrl <- parseBaseUrl "http://localhost:1337/"
    let clientEnv = mkClientEnv manager baseUrl
    r <- flip runClientM clientEnv $ do
        descs <- listDescriptions
        liftIO $ mapM_ TIO.putStrLn descs
        liftIO $ putStrLn "Enter number of desired secret: "
        choice <- liftIO $ read <$> getLine
        --
        (token, pse, modulo) <- buy
        obfs <- getRow token choice
        -- generate queries
        gen0 <- liftIO getStdGen
        let (guessed, queries, gen1) = queryMessage modulo obfs pse gen0
        liftIO $ setStdGen gen1
        --
        sequenceA <$> mapM (verify token) queries >>= \case
          Nothing -> liftIO . putStrLn $ "Error: not enought tries to get whole secret"
          Just responses ->
            let secret = assembleBits $ zipWith (==) responses guessed
            in liftIO . BS.putStrLn $ secret
    --
    case r of
        Left err -> print err
        Right x  -> pure x
