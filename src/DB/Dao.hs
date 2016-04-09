{-# LANGUAGE OverloadedStrings #-}


module DB.Dao where

import Web.Scotty.Internal.Types (ActionT)
import Database.PostgreSQL.Simple
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Control.Monad.IO.Class (liftIO)
import Emb.Entity.Person
import GHC.Int(Int64)
import Data.Aeson
-- import Control.Monad.IO.Class

-------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- Transactions.
--

-- Update database
execSqlT :: ToRow q => Pool Connection -> q -> Query -> IO Int64
execSqlT pool args sql = withResource pool ins
       where ins conn = withTransaction conn $ execute conn sql args


insertPerson :: Pool Connection -> Maybe Person -> ActionT TL.Text IO ()
insertPerson pool Nothing = return ()
insertPerson pool (Just (Person _ firstName lastName email)) = do
     liftIO $ execSqlT pool [firstName, lastName, email] 
                            insertPersonQ
     return ()

insertPersonQ :: Query
insertPersonQ = "INSERT INTO person(firstName, lastName, email) VALUES(?,?,?)"

