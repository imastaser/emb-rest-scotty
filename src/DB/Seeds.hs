{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module DB.Seeds
      (seed) where

import DB.Dao(runDb)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types(Query)
import Control.Monad (forM_)
import Data.Pool(Pool)
import GHC.Int(Int64)

seed :: Pool Connection -> IO ()
seed pool = do 
 runDb pool $ \c -> do
    forM_ seeds (runSeed c)

runSeed :: Connection -> Query -> IO Int64
runSeed c q = execute_ c q

seeds :: [Query]
seeds = [ workTypeQ ]

-- https://stackoverflow.com/questions/1109061/insert-on-duplicate-update-in-postgresql

workTypeQ :: Query
workTypeQ = [sql|
WITH new_values (name, note) as (
  values 
     ('Ասեղնագործ', 'ասեղնագործություն'),
     ('Դիզայն', 'պատկերի թվային ծրագրավորում ասեղնագործ մեքենաների համար')
)
INSERT INTO worktype (name, note)
SELECT name, note
FROM new_values
WHERE NOT EXISTS (SELECT 1 
                  FROM worktype)
|]