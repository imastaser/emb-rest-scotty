{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- Non type-variable argument in the constraint: FromRow (Only b)  (Use FlexibleContexts to permit this)
{-# LANGUAGE FlexibleContexts  #-}

module Migrate.PostgreSQL (migrate1, metaPool, mkDB) where

import Data.Pool
import Data.Maybe
import Control.Monad (void, unless, forM_)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- void $ execute_  c createDBQ  -- (Only "embroidery_development" :: Only String)
mkDB pool = withResource pool createDB
                where createDB conn = execute_  conn createDBQ


migrate1 :: Pool Connection -> IO ()
migrate1 pool = do 
 -- void $ mkDB pool
 runDb pool $ \c -> do
    void $ execute_ c bootstrapQ
    forM_ migrations (runMigration c)

runMigration :: Connection -> (Int, Query) -> IO ()
runMigration c (v, q) = do
  e <- exists c [sql| SELECT TRUE FROM schema_migration WHERE migration = ? |] (Only v)
  unless e $ do
     void $ execute c [sql| INSERT INTO schema_migration (migration) VALUES (?) |] (Only v)
     void $ execute_ c q


migrations :: [(Int, Query)]
migrations = [
    (1, personQ)
  , (2, productQ)
  ]

runDb :: Pool Connection -> (Connection -> IO a) -> IO a
runDb pool f =
  withResource pool (\conn -> withTransaction conn (f conn))

exists :: (ToRow a) => Connection -> Query -> a ->  IO Bool
exists c q v =
  isJust <$> (fetch' c q v :: IO (Maybe Bool))


list :: (ToRow a, FromRow b) => Connection -> Query -> a ->  IO [b]
list c q v = query c q v

fetch :: (ToRow a, FromRow b) => Connection -> Query -> a -> IO (Maybe b)
fetch c q v =
  fmap listToMaybe $ list c q v

fetch' :: (ToRow a, FromRow (Only b)) => Connection -> Query -> a ->  IO (Maybe b)
fetch' c q v =
  fmap (fmap fromOnly) $ fetch c q v

-- getMigrations :: Connection -> IO [ChangeHistory]
getMigrations conn = query_ conn migrationsQ

-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------

bootstrapQ :: Query
bootstrapQ = [sql|
CREATE TABLE IF NOT EXISTS schema_migration (
    id              serial      NOT NULL,
    migration       int         NOT NULL,
    time            timestamptz NOT NULL DEFAULT now(),

    PRIMARY KEY (id),
    UNIQUE (migration)
);
|]


createDBQ :: Query
createDBQ = [sql|
CREATE DATABASE embroidery_development
  WITH OWNER = postgres
       ENCODING = 'UTF8'
       TABLESPACE = pg_default
       LC_COLLATE = 'English_United States.1252'
       LC_CTYPE = 'English_United States.1252'
       CONNECTION LIMIT = -1;
|]

createDBQ' :: Query
createDBQ' = [sql|
DO
$do$
BEGIN

IF EXISTS (SELECT 1 FROM pg_database WHERE datname = 'embroidery_development') THEN
   RAISE NOTICE 'Database already exists'; 
ELSE
   PERFORM dblink_exec('dbname=' || current_database()  -- current db
                     , 'CREATE DATABASE embroidery_development');
END IF;

END
$do$
|]


personQ :: Query
personQ = [sql|
CREATE TABLE IF NOT EXISTS person (
    id              serial      NOT NULL,
    firstName       text        NOT NULL,
    lastName        text        NULL,
    time            timestamptz NOT NULL DEFAULT now(),

    PRIMARY KEY (id)
);
|]

productQ :: Query
productQ = [sql|
CREATE TABLE IF NOT EXISTS product (
    id              serial NOT NULL,
    name            text   NOT NULL,
    price           text   NULL,
    time            timestamptz NOT NULL DEFAULT now(),

    PRIMARY KEY (id)
);
|]





migrationsQ :: Query
migrationsQ =
  "SELECT id, name, description, time FROM schema_migration ORDER BY id;"

hello :: IO Int
hello = do
  pool <- createPool (connect defaultConnectInfo
                       { 
                         connectUser     = "postgres"
                       , connectPassword = "Welcome*99"
                       }) close 1 40 10
  runDb pool $ \conn -> do
  -- void $ execute_ 
  [Only i] <- query_ conn "select 2 + 2"
  return i


metaPool = createPool (connect defaultConnectInfo
                       { 
                         connectUser     = "postgres"
                       , connectPassword = "Welcome*99"
                       }) close 1 40 10