{-# LANGUAGE OverloadedStrings, FlexibleContexts, QuasiQuotes  #-}
module DB.Migrate where

import Control.Applicative
import Control.Monad

import qualified Data.ByteString.Char8 as C8
import Data.Pool
import Data.Maybe
import Data.Monoid (mconcat)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

{--import Web.Scotty

go :: Int -> String -> IO ()
go port db =
  mkPool db >>= \pool -> migrate pool  >> (scotty port $ route pool)

route :: Pool Connection -> ScottyM ()
route pool = do
  post "/build/:library/:version" $ do
    library <- param "library"
    version <- param "version"
    x <- runDb pool $ \c -> do
      error "todo"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
--}
migrate :: Pool Connection -> IO ()
migrate pool = runDb pool $ \c -> do
  void $  execute_ c [sql|CREATE TABLE IF NOT EXISTS migration_info (migration INT PRIMARY KEY)|]
  forM_ migrations (runMigration c)

runMigration :: Connection -> (Int, Query) -> IO ()
runMigration c (v, q) = do
  e <- exists c [sql| SELECT TRUE FROM migration_info WHERE migration = ? |] (Only v)
  unless e $ do
     void $ execute c [sql| INSERT INTO migration_info (migration) VALUES (?) |] (Only v)
     void $ execute_ c q

list :: (ToRow a, FromRow b) => Connection -> Query -> a ->  IO [b]
list c q v = query c q v

fetch :: (ToRow a, FromRow b) => Connection -> Query -> a -> IO (Maybe b)
fetch c q v =
  fmap listToMaybe $ list c q v

fetch' :: (ToRow a, FromRow (Only b)) => Connection -> Query -> a ->  IO (Maybe b)
fetch' c q v =
  fmap (fmap fromOnly) $ fetch c q v

exists :: (ToRow a) => Connection -> Query -> a ->  IO Bool
exists c q v =
  isJust <$> (fetch' c q v :: IO (Maybe Bool))

runDb :: Pool Connection -> (Connection -> IO a) -> IO a
runDb pool f =
  withResource pool (\conn -> withTransaction conn (f conn))

mkPool :: String -> IO (Pool Connection)
mkPool cfg =
  createPool (connectPostgreSQL . C8.pack $ cfg) close 1 20 10


migrations :: [(Int, Query)]
migrations = [
    (1, "CREATE TABLE library (id integer NOT NULL PRIMARY KEY, name character varying NOT NULL)")
  , (2, "CREATE TABLE build_number (id integer NOT NULL PRIMARY KEY, library integer NOT NULL, version character varying NOT NULL, build integer NOT NULL default -1)")
  ]