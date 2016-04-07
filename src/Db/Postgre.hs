{-# LANGUAGE OverloadedStrings #-}

module DB.Postgre where


import Emb.Types (Environment(..), DbConfig(..))

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField (toField)  



import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Data.Configurator     as C
import qualified Data.Text             as T

import GHC.Int (Int64)



-- connString :: BS8.ByteString
connString = BS8.pack $ unwords [ "host='localhost'"
                        , " port=5432"
                        , " dbname='test'"
                        , " user='postgres'"
                        , " password='Welcome*99'"
                        ]


parseConfig :: Environment -> FilePath -> IO DbConfig
parseConfig e configFile =
    do config   <- C.load [C.Required configFile]
       host     <- C.require config "host"
       port     <- C.require config "port"
       database <- C.require config "database" :: IO T.Text
       user     <- C.require config "user"
       password <- C.require config "password"
       let (dbEnv, n) = case e of
                            Development -> ("_development", 1)
                            Production  -> ("_production", 8)
                            Test        -> ("_test", 1)
       return $ mkDbConfig host port (T.append database dbEnv) user password n

mkDbConfig = DbConfig


newConn :: DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo
                       { connectHost     = T.unpack $ dbHost conf
                       , connectPort     = fromIntegral  $ dbPort conf
                       , connectDatabase = T.unpack $ dbName conf
                       , connectUser     = T.unpack $ dbUser conf
                       , connectPassword = T.unpack $ dbPassword conf
                       }

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
