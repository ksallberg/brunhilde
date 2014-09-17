{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}

module BattleShip where

import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Maybe
import           GHC.Generics
import           Network.HTTP.Conduit
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
    } deriving (Generic)

data State = State
    { shots  :: [[Int]]
    , player :: RequestData
    } deriving (Show)

type PlayerName = String
type Route      = String

instance FromJSON ResponseData
instance FromJSON Radar

instance Show Radar where
    show (Radar board won) = "Radar feedback\n" ++
                             "Won? " ++ (show won) ++ "\n" ++
                             (concat [line ++ "\n"| line <- board])

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
    let user = RequestData{player_name = "berra", shoot_at = Nothing}
        initialState = State{shots = [[2,1]], player = user}
    gameLoop initialState

gameLoop :: State -> IO ()
gameLoop (State shots player) = do
    let shot = RequestData{player_name = player_name player,
                           shoot_at = Just [2,1]}
    -- Register player
    regPlayer <- signup player
    putStrLn $ show regPlayer
    -- Do radar
    radar1 <- radar player
    putStrLn $ show radar1
    -- Shoot
    sht <- shoot shot
    putStrLn $ show sht
    -- Radar again
    radar2 <- radar player
    putStrLn $ show radar2

signup :: RequestData -> IO ResponseData
signup user = do body <- sendAndReadResponse user "/battleship/register/"
                 return $ fromJust ((decode body) :: Maybe ResponseData)

shoot :: RequestData -> IO ResponseData
shoot user = do body <- sendAndReadResponse user "/battleship/shoot/"
                return $ fromJust ((decode body) :: Maybe ResponseData)

radar :: RequestData -> IO Radar
radar user = do body <- sendAndReadResponse user "/battleship/radar/"
                return $ fromJust ((decode body) :: Maybe Radar)

sendAndReadResponse :: RequestData -> Route -> IO LBS8.ByteString
sendAndReadResponse req route =
    do let usr = toJson req
       response <- makeRequest (hostName++route) (LBS8.pack usr)
       return $ responseBody response

makeRequest :: (MonadThrow m, MonadIO m, MonadBaseControl IO m) =>
               String -> LBS8.ByteString -> m (Response LBS8.ByteString)
makeRequest url reqBody =
    do req <- parseUrl url
       withManager $ httpLbs req
                  { method      = "GET"
                  , requestBody = RequestBodyLBS reqBody
                  }
