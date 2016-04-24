{-# LANGUAGE OverloadedStrings #-}


module DB.Dao
      ( execSqlT
      , fetch1
      , fetchSimple1)
     where

import Database.PostgreSQL.Simple
import Data.Pool(Pool, createPool, withResource)


import GHC.Int(Int64)
import Data.Aeson


-------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- Transactions.
--

-- Update database
execSqlT :: ToRow q => Pool Connection -> q -> Query -> IO Int64
execSqlT pool args sql = withResource pool ins
       where ins conn = withTransaction conn $ execute conn sql args

--------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- No transactions.
--
-- Accepts arguments
fetch1 :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetch1 pool args sql = withResource pool retrieve
      where retrieve conn = query conn sql args


-- No arguments -- just pure sql
fetchSimple1 :: FromRow r => Pool Connection -> Query -> IO [r]
fetchSimple1 pool sql = withResource pool retrieve
       where retrieve conn = query_ conn sql