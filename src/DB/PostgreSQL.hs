{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoImplicitPrelude #-}


module DB.PostgreSQL where


import Emb.Types (DefaultEnv(..), DbConfig(..))

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField (toField)  

import Data.Pool(Pool, createPool, withResource)


import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Data.Configurator     as C
import qualified Data.Text             as T

import GHC.Int (Int64)


-- connString :: BS8.ByteString
-- connString :: Data.ByteString.Internal.ByteString
connString = BS8.pack $ unwords [ "host='localhost'"
                        , " port=5432"
                        , " dbname='test'"
                        , " user='postgres'"
                        , " password='Welcome*99'"
                        ]

-- | parse postgresql.config and make DbConfig for given environment
parseConfig :: DefaultEnv -> FilePath -> IO DbConfig
parseConfig e configFile =
    do config   <- C.load [C.Required configFile]
       host     <- C.require config "host"
       port     <- C.require config "port"
       database <- C.require config "database" :: IO T.Text
       user     <- C.require config "user"
       password <- C.require config "password"
       maxAlive <- C.require config "maxalive"
       stripes  <- C.require config "stripes"
       let (dbEnv, n) = case e of
                          Development -> ("_development", 1)
                          Production  -> ("_production", 8)
                          Staging     -> ("_production", 8)
                          Testing     -> ("_test", 1)
       return $ DbConfig
                  {
                    dbHost        = host
                  , dbPort        = port
                  , dbName        = (T.append database dbEnv)
                  , dbUser        = user
                  , dbPassword    = password
                  , dbStripes     = stripes
                  , dbMaxAlive    = maxAlive
                  , dbConnections = n 
                }

-- | create postgresql connection from DbConfig
-- It is needed to use with resource pool
newConn :: DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo
                       { connectHost     = T.unpack $ dbHost conf
                       , connectPort     = fromIntegral  $ dbPort conf
                       , connectDatabase = T.unpack $ dbName conf
                       , connectUser     = T.unpack $ dbUser conf
                       , connectPassword = T.unpack $ dbPassword conf
                       }

-- | create connection pool
mkPool :: DbConfig -> IO (Pool Connection)
mkPool cfg = createPool (newConn cfg) close (dbStripes cfg) 40 (dbConnections cfg)




testPg :: Int -> Int -> IO Int
testPg a b = do
  {--conn <- connect defaultConnectInfo {
    connectDatabase = "test"
  , connectUser = "postgre"
  , conectPassword = "welcome"
  }--}
  conn <- connectPostgreSQL connString
  putStrLn "2 + 2"
  -- mapM_ print =<< ( query_ conn "select 2 + 2" :: IO [Only Int] )
  [Only i] <- query conn "select ? + ?" (a,b) :: IO [Only Int]
  return i

insert :: IO Int64
insert = do
  conn <- connectPostgreSQL connString
  execute conn "insert into s (tt, note) values (?,?)" 
      $ [toField ("user"::String), toField ("note"::String)]
