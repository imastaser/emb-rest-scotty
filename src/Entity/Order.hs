{-# OPTIONS -Wall #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | The product entity.

module Entity.Order
        ( Order(..)
        , personOrders
        , insertOrder
        , allOrders
        --, deleteOrder
        --, findOrder
        --, updateOrder
        )
       where

import DB.Dao (fetch1, fetchSimple1, execSqlT)
import Data.Pool (Pool, withResource)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Web.Scotty.Internal.Types (ActionT)

import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Data.ByteString.Lazy.Char8 as BL

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow

import Data.Time.Clock (UTCTime)

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
-- import Database.PostgreSQL.Simple.FromRow
-- import Database.PostgreSQL.Simple.FromField


import Data.Aeson 
import GHC.Int(Int64) 

data Order = Order
              {
                 orderId            :: Int64
               , person_Id          :: Int64
               , product_Id         :: Int64
               , price              :: Int64
               , caxs               :: Int64
               , count              :: Int64
               , datetime           :: TL.Text --UTCTime
               , note               :: TL.Text
              } 
              deriving (Show)

instance FromRow Order where
  fromRow = Order <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field

instance ToRow Order where
  toRow order =
    [ toField (person_Id order)
    , toField (product_Id order)
    , toField (price order)
    , toField (caxs  order)
    , toField (count  order)
    , toField (datetime  order)
    , toField (note order)
    ]


instance FromJSON Order where
  parseJSON (Object v) = 
    Order <$>
            v .:?  "id" .!= 0     <*> -- the field "id" is optional
            v .:   "person_id"    <*>
            v .:   "product_id"   <*>
            v .:   "price"        <*>
            v .:   "caxs"         <*>
            v .:   "count"        <*>
            v .:   "datetime"     <*>
            v .:   "note"     

instance ToJSON Order where
     toJSON (Order _id _person_id _product_id _price _caxs _count _datetime _note) =
         object ["id"           .= _id,
                 "person_id"    .= _person_id,
                 "product_id"   .= _product_id,
                 "price"        .= _price,
                 "caxs"         .= _caxs,
                 "count"        .= _count,
                 "datetime"     .= _datetime,
                 "note"         .= _note]   


insertOrder :: Pool Connection -> Maybe Order -> Int64 -> IO [Only Int64]
insertOrder _ Nothing _ = return [Only 0]
insertOrder pool (Just order) _ = fetch1 pool order insertQ 
    where 
        insertQ :: Query
        insertQ = [sql|INSERT INTO "order"
                       (person_id, product_id, price, caxs, count, datetime, note)
                        VALUES(?,?,?,?,?,?,?) RETURNING id|]  

personOrders :: Pool Connection -> Int -> IO [Order]
personOrders pool pid = do
     res <- fetch1 pool (Only pid) personOrdersQ :: IO [Order] 
     return res 
     where 
      personOrdersQ :: Query
      personOrdersQ = [sql|
                            SELECT id, person_id, product_id, price, caxs, count, datetime, note
                            FROM "order" 
                            WHERE person_id = ? |]   


allOrders :: Pool Connection -> IO [Order]
allOrders pool = do
     res <- fetchSimple1 pool allOrdersQ :: IO [Order] 
     return  res

-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------
allOrdersQ :: Query
allOrdersQ = [sql|
                SELECT id, person_id, product_id, price, caxs, count, datetime, note 
                FROM "order"   |]

              
