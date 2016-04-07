{-# LANGUAGE OverloadedStrings #-}

module DB.Core
      ()
      where

import Emb.Types (Environment(..), DbConfig(..))

import        DB.Postgre (connString)    
import        Data.Pool
import        Database.PostgreSQL.Simple (connectPostgreSQL, close, Connection)



-- |"Striped" means that a single 'Pool' consists of several
-- sub-pools, each managed independently.  A single stripe is fine for
-- many applications, and probably what you should choose by default.
-- More stripes will lead to reduced contention in high-performance
-- multicore applications, at a trade-off of causing the maximum
-- number of simultaneous resources in use to grow

-- | connection pool
-- (connect connectInfo)  - open action
-- close                  - close action
-- 1                      - stripes, a single stripe is fine for many applications 
-- 20                     - max keep alive (s)
-- 10                     - max connection, a good baseline is two times the number of cores 






-- getPool :: Environment -> IO (Pool Connection)
{--getPool e = do
   -- s <- getConnectionString e
   let s = connString
   let n = getConnectionSize e
   case e of
     Development -> runStdoutLoggingT (mkPool s n)
     Production  -> runStdoutLoggingT (mkPool s n)
     Test        -> runNoLoggingT     (mkPool s n)
--}



-- mkPool :: String -> Int -> IO (Pool Connection)
mkPool s n = createPool (connectPostgreSQL s) close 1 20 n


getConnectionSize :: Environment -> Int
getConnectionSize Development = 1
getConnectionSize Production = 8
getConnectionSize Test = 1
