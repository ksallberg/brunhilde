{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}

module BattleShip where

import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Maybe
import           GHC.Generics
import           Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS

import qualified Data.ByteString.Lazy.Char8 as LBS8

data RequestData = RequestData
    { player_name :: String
    , shoot_at    :: Maybe [Int]
    } deriving (Show, Generic)

data ResponseData = ResponseData
    { status :: String
    } deriving (Show, Generic)

data Radar = Radar
    { game_board :: [String]
    , won        :: String
    } deriving (Show, Generic)

--instance ToJSON RequestData where
--    toJSON (RequestData name Nothing)  = object ["player_name" .= name]
--    toJSON (RequestData name (Just x)) = object ["player_name" .= name,
--                                                 "shoot_at"    .= x]

instance FromJSON ResponseData
instance FromJSON Radar

toJson (RequestData name Nothing)    =
    "{\"player_name\":\"" ++ name ++"\"}"
toJson (RequestData name (Just shot)) =
    "{\"player_name\":\"" ++ name ++ "\", \"shoot_at\":" ++ (show shot) ++ "}"

main :: IO ()
main = do
    putStrLn "Making HTTP request"
    -- register
    let host    = "http://localhost:2222"
        usr     = toJson $ RequestData { player_name = "berra"
                                       , shoot_at = Nothing}
        shot    = toJson $ RequestData { player_name = "berra"
                                       , shoot_at = Just [3, 2]}
    reg_resp   <- makeRequest (host++"/battleship/register/") (LBS8.pack usr)
    radar_resp <- makeRequest (host++"/battleship/radar/")    (LBS8.pack usr)
    shot_resp  <- makeRequest (host++"/battleship/shoot/")    (LBS8.pack shot)
    print $ responseBody reg_resp
    print $ responseBody radar_resp
    print $ responseBody shot_resp

makeRequest :: (MonadThrow m, MonadIO m, MonadBaseControl IO m) =>
               String -> LBS.ByteString -> m (Response LBS.ByteString)
makeRequest url reqBody =
    do req <- parseUrl url
       withManager $ httpLbs req
                  { method      = "GET"
                  , requestBody = RequestBodyLBS reqBody
                  }
