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
    { board :: [String]
    , won   :: String
    } deriving (Show, Generic)

type PlayerName = String

instance FromJSON ResponseData
instance FromJSON Radar

toJson :: RequestData -> String
toJson (RequestData name Nothing) =
    "{\"player_name\":\"" ++ name ++ "\"}"
toJson (RequestData name (Just shot)) =
    "{\"player_name\":\"" ++ name ++ "\", \"shoot_at\":" ++ (show shot) ++ "}"

hostName :: String
hostName = "http://localhost:2222"

main :: IO ()
main = do
    putStrLn "Making HTTP request"
    regPlayer <- registerPlayer "berra"
    radar     <- radarPlayer    "berra"
    sht       <- shootPlayer    "berra" [2,3]
    putStrLn $ show regPlayer
    putStrLn $ show radar
    putStrLn $ show sht

registerPlayer :: PlayerName -> IO ResponseData
registerPlayer name =
    do let usr = toJson $ RequestData { player_name = name
                                      , shoot_at = Nothing}
       response <- makeRequest (hostName++"/battleship/register/")
                               (LBS8.pack usr)
       let m = decode (responseBody response) :: Maybe ResponseData
       return $ fromJust m

radarPlayer :: PlayerName -> IO Radar
radarPlayer name =
    do let usr = toJson $ RequestData { player_name = name
                                      , shoot_at = Nothing}
       response <- makeRequest (hostName++"/battleship/radar/")
                               (LBS8.pack usr)
       let m = decode (responseBody response) :: Maybe Radar
       return $ fromJust m

shootPlayer :: PlayerName -> [Int] -> IO ResponseData
shootPlayer name coord =
    do let shot    = toJson $ RequestData { player_name = name
                                          , shoot_at    = Just coord}
       radar_resp <- makeRequest (hostName++"/battleship/shoot/")
                                 (LBS8.pack shot)
       let m = decode (responseBody radar_resp) :: Maybe ResponseData
       return $ fromJust m

makeRequest :: (MonadThrow m, MonadIO m, MonadBaseControl IO m) =>
               String -> LBS.ByteString -> m (Response LBS.ByteString)
makeRequest url reqBody =
    do req <- parseUrl url
       withManager $ httpLbs req
                  { method      = "GET"
                  , requestBody = RequestBodyLBS reqBody
                  }
