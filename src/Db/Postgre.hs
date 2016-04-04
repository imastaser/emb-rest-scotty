module Db.Postgre where

import Database.PostgreSQL.Simple
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as BS8 (pack)
import GHC.Int (Int64)
import Database.PostgreSQL.Simple.ToField (toField)  

-- connString :: BS8.ByteString
connString = BS8.pack $ unwords [ "host='localhost'"
                        , " port=5432"
                        , " dbname='test'"
                        , " user='postgres'"
                        , " password='welcome'"
                        ]

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
