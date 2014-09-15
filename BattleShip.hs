{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}

module BattleShip where

import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class
import           Data.Aeson
import           GHC.Generics
import           Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS

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

instance ToJSON RequestData where
    toJSON (RequestData name Nothing)  = object ["player_name" .= name]
    toJSON (RequestData name (Just x)) = object ["player_name" .= name,
                                                 "shoot_at"    .= x]

instance FromJSON ResponseData
instance FromJSON Radar

main :: IO ()
main = do
    putStrLn "Making HTTP request"
    -- register
    let usr   = RequestData {player_name = "berra", shoot_at = Nothing}
        shot  = RequestData {player_name = "berra", shoot_at = Just [3, 2]}
    putStrLn (show $ encode shot)
    reg_resp   <- makeRequest "http://192.168.1.89:2222/battleship/register/"
                              (encode usr)
--    shot_resp  <- makeRequest "http://192.168.1.89:2222/battleship/shoot/"
--                              (encode shot)
    radar_resp <- makeRequest "http://192.168.1.89:2222/battleship/radar/"
                              (encode usr)
    print $ responseBody reg_resp
    --print $ responseBody shot_resp
    print $ responseBody radar_resp

makeRequest :: (MonadThrow m, MonadIO m, MonadBaseControl IO m) =>
               String -> LBS.ByteString -> m (Response LBS.ByteString)
makeRequest url reqBody =
    do req <- parseUrl url
       withManager $ httpLbs req
                  { method      = "GET"
                  , requestBody = RequestBodyLBS reqBody
                  }
