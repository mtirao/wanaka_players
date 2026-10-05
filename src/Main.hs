{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Proxy (Proxy (Proxy))
import Data.Text ( Text, pack )
import Network.Wai (Application)
import Network.Wai.Handler.Warp ( run, run )
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Server (app)
import qualified Hasql.Connection.Setting as Setting
import qualified Hasql.Connection.Setting.Connection as ConnSetting
import Hasql.Pool as P
import qualified Hasql.Pool.Config as PoolConfig
import Servant

import PlayerDTO

data DbConfig = DbConfig
    { dbName     :: String
    , dbUser     :: String
    , dbPassword :: String
    , dbHost     :: String
    , dbPort     :: Int
    }

makeDbConfig :: CT.Config -> IO (Maybe DbConfig)
makeDbConfig conf = do
    dbConfname <- C.lookup conf "database.name" :: IO (Maybe String)
    dbConfUser <- C.lookup conf "database.user" :: IO (Maybe String)
    dbConfPassword <- C.lookup conf "database.password" :: IO (Maybe String)
    dbConfHost <- C.lookup conf "database.host" :: IO (Maybe String)
    dbConfPort <- C.lookup conf "database.port" :: IO (Maybe Int)
    return $ DbConfig <$> dbConfname
                      <*> dbConfUser
                      <*> dbConfPassword
                      <*> dbConfHost
                      <*> dbConfPort

-- Servant API for the profile endpoints

main :: IO ()
main = do
    loadedConf <- C.load [C.Required "application.conf"]
    dbConf <- makeDbConfig loadedConf
    case dbConf of
        Nothing -> putStrLn "Error loading configuration"
        Just conf -> do
            let connString = pack $ "host=" ++ dbHost conf ++ " port=" ++ show (dbPort conf)
                                ++ " user=" ++ dbUser conf ++ " password=" ++ dbPassword conf
                                ++ " dbname=" ++ dbName conf
                connSettings = Setting.connection (ConnSetting.string connString)
            pool <- P.acquire $ PoolConfig.settings
                [ PoolConfig.size 10
                , PoolConfig.acquisitionTimeout 60
                , PoolConfig.agingTimeout 60
                , PoolConfig.idlenessTimeout 60
                , PoolConfig.staticConnectionSettings [connSettings]
                ]
            putStrLn "Starting Servant server on port 3001 "
            run 3010 (app pool)
