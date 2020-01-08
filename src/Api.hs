{-# LANGUAGE DataKinds, DeriveGeneric, PolyKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE LambdaCase                 #-}

module Api
( Token (Token)
, AndosApi, andosApi
) where

import Data.Text   (Text)
import Data.Yaml   (FromJSON, ToJSON)
import Data.Proxy  (Proxy (Proxy))
import Servant.API ( (:>),  Capture, ReqBody, Get, Post, JSON, (:<|>)
                   , FromHttpApiData, ToHttpApiData
                   )


newtype Token = Token Int
    deriving (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

type AndosApi =
         "list" :> Get  '[JSON] [Text]
    :<|> "buy"  :> Post '[JSON] (Token, Integer, Integer)
    :<|> "getRow" :> Capture "token" Token :> ReqBody '[JSON] Int :> Get '[JSON] [Integer]
    :<|> "verify" :> Capture "token" Token :> ReqBody '[JSON] Integer
        :> Post '[JSON] (Maybe Bool)

andosApi :: Proxy AndosApi
andosApi = Proxy
