{-# LANGUAGE OverloadedStrings #-}


module DB.Dao where

import Database.PostgreSQL.Simple
import Data.Pool(Pool, createPool, withResource)


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



--------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- No transactions.
--
-- Accepts arguments
fetch1 :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetch1 pool args sql = withResource pool retrieve
      where retrieve conn = query conn sql args


