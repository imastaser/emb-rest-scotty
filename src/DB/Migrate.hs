{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
-- Non type-variable argument in the constraint: 
-- FromRow (Only b)  (Use FlexibleContexts to permit this)
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables  #-}



module DB.Migrate (migrate, mkDB) where

import DB.SchemeQuery
import Init.Types

import Data.Pool
import Data.Maybe
import Control.Monad (void, unless, when, forM_)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Data.String(fromString)

import qualified Data.Text as T


-- void $ execute_  c createDBQ  -- (Only "embroidery_development" :: Only String)
mkDB :: DbConfig -> IO ()
mkDB cfg  = do 
    conn <- connect defaultConnectInfo
                       { 
                         connectUser     = T.unpack $ dbUser cfg
                       , connectPassword = T.unpack $ dbPassword cfg
                       }
    xs :: [Report] <- query conn 
                      [sql| SELECT datname FROM pg_database 
                            WHERE datname = ? 
                      |] (Only (dbName cfg))
    when (null $ xs) $ do 
        void $ execute_ conn (
          fromString $
                    "CREATE DATABASE " ++ (T.unpack $ dbName cfg) ++
                    " WITH OWNER = postgres   \
                    \  ENCODING = 'UTF8'      \
                    \ TABLESPACE = pg_default \
                    \ CONNECTION LIMIT = -1;"
                  )

        -- createDBQ (Only (dbName cfg))
   
{--mkDB pool = withResource pool createDB
               where createDB conn = execute_  conn createDBQ
--}
       


migrate :: Pool Connection -> IO ()
migrate pool = do 
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
  , (3, workTypeQ)
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
getMigrations :: forall r. FromRow r => Connection -> IO [r]
getMigrations conn = query_ conn migrationsQ

-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------



-- numeric(15,2)
changeColumnType :: Query
changeColumnType = "ALTER TABLE person ALTER COLUMN firstname TYPE character varying(32)"

migrationsQ :: Query
migrationsQ =
  "SELECT id, name, description, time FROM schema_migration ORDER BY id;"

data Report = Report { datname :: T.Text} deriving (Show)

instance FromRow Report where
  fromRow = Report <$> field 

instance ToRow Report where
  toRow d = [toField (datname d)]

instance ToField Report where
  toField d = toField (datname d)   

hello :: (ToField Report, FromRow Report) => String ->  IO [Report]
hello dbname = do
  pool <- createPool (connect defaultConnectInfo
                       { 
                         connectUser     = "postgres"
                       , connectPassword = "Welcome*99"
                       }) close 1 40 10
  -- [Only k] :: [Only Int]
  -- xs <- withResource pool $
  conn <- connect defaultConnectInfo
                       { 
                         connectUser     = "postgres"
                       , connectPassword = "Welcome*99"
                       }

 -- execute_ conn ((fromString ("CREATE DATABASE " ++ "dbname")) :: Query)                    
  
  _ <- execute_ conn (
          fromString $
                        "CREATE DATABASE " ++ dbname ++
                        " WITH OWNER = postgres \
                        \ TABLESPACE = pg_default \
                        \ CONNECTION LIMIT = -1;"
                      )

  query conn [sql| SELECT datname FROM pg_database 
                    WHERE datname = ? 
                  |] (Only "test" :: Only T.Text)

                                    
  -- query_ conn  ifdbExistsQ 
  --xs <- query_ conn ifdbExistsQ
  --forM_ xs $ \(name) ->
   -- putStrLn  name 
  -- runDb pool $ \conn -> do
  -- void $ execute_ 
  -- [Only i] <- query_ conn "select 2 + 2"
  --return xs
  
{--
mkDB pool = do 
      [Only i] :: [Only Int] <- withResource pool ka 
      unless (i == 0) $ do 
        void $ withResource pool createDB
  where
   createDB conn = execute_  conn createDBQ'

ka conn       = query_ conn  ifdbExistsQ


--}


