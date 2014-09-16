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
type Route = String

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
    let user = RequestData{ player_name = "berra", shoot_at = Nothing}
        shot = RequestData{ player_name = "berra", shoot_at = Just [2,1]}
    -- Register player
    regPlayer <- signup user
    putStrLn $ show regPlayer
    -- Do radar
    radar1 <- radar user
    putStrLn $ show radar1
    -- Shoot
    sht <- shoot shot
    putStrLn $ show sht
    -- Radar again
    radar2 <- radar user
    putStrLn $ show radar2

signup user   = readResponse user "/battleship/register/"
shoot user = readResponse user "/battleship/shoot/"

readResponse :: RequestData -> Route -> IO ResponseData
readResponse req route =
    do let usr = toJson req
       response <- makeRequest (hostName ++ route) (LBS8.pack usr)
       let m = decode (responseBody response) :: Maybe ResponseData
       return $ fromJust m

radar :: RequestData -> IO Radar
radar req =
    do let usr = toJson req
       response <- makeRequest (hostName++"/battleship/radar/")
                               (LBS8.pack usr)
       let m = decode (responseBody response) :: Maybe Radar
       return $ fromJust m

makeRequest :: (MonadThrow m, MonadIO m, MonadBaseControl IO m) =>
               String -> LBS.ByteString -> m (Response LBS.ByteString)
makeRequest url reqBody =
    do req <- parseUrl url
       withManager $ httpLbs req
                  { method      = "GET"
                  , requestBody = RequestBodyLBS reqBody
                  }
