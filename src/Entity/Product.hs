{-# OPTIONS -Wall #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | The product entity.

module Entity.Product
        ( Product(..)
        , personProducts
        , insertProduct
        , allProducts)
       where

import DB.Dao (fetch1, fetchSimple1)
import Data.Pool(Pool, withResource)

import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Data.ByteString.Lazy.Char8 as BL

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
-- import Database.PostgreSQL.Simple.FromRow
-- import Database.PostgreSQL.Simple.FromField


import Data.Aeson 
import GHC.Int(Int64) 

data Product = Product
              {
                 productId         :: Int64
               , person_Id         :: Int64
               , workType_Id       :: Int64
               , productName       :: TL.Text
               , productPrice      :: Int64
               , caxs              :: Int64
               , productNote       :: TL.Text
              } 
              deriving (Show)

instance FromRow Product where
  fromRow = Product <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field

instance ToRow Product where
  toRow product =
    [ toField (person_Id product)
    , toField (workType_Id product)
    , toField (productName  product)
    , toField (productPrice  product)
    , toField (caxs  product)
    , toField (productNote product)
    ]

instance FromJSON Product where
  parseJSON (Object v) = 
    Product <$>
            v .:?  "id" .!= 0     <*> -- the field "id" is optional
            v .:   "person_id"    <*>
            v .:   "workType_id"  <*>
            v .:   "name"         <*>
            v .:   "price"        <*>
            v .:   "caxs"         <*>
            v .:   "note"     

instance ToJSON Product where
     toJSON (Product _id _person_id _workType_id _name _price _caxs _note) =
         object ["id"           .= _id,
                 "person_id"    .= _person_id,
                 "workType_id"  .= _workType_id,
                 "name"         .= _name,
                 "price"        .= _price,
                 "caxs"         .= _caxs,
                 "note"         .= _note]   


{-- postProduct :: (HasPostgres m) => T.Text -> Int -> PersonId -> m Int
postProduct name price person = do
    productids <- returning insertQ [(name, price, person)]
    return $ head productids                    
--}

insertProduct :: Pool Connection -> Maybe Product -> Int64 -> IO [Only Int64]
insertProduct _ Nothing _ = return [Only 0]
insertProduct pool (Just product) _ = fetch1 pool product insertQ 

personProducts :: Pool Connection -> Int -> IO [Product]
personProducts pool pid = do
     res <- fetch1 pool (Only pid) personProductsQ :: IO [Product] 
     return  res 


allProducts :: Pool Connection -> IO [Product]
allProducts pool = do
     res <- fetchSimple1 pool allProductQ :: IO [Product] 
     return  res

-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------
allProductQ :: Query
allProductQ = [sql|
                SELECT id, person_id, workType_id, name, price, caxs, note 
                FROM product |]

insertQ :: Query
insertQ = [sql|INSERT INTO product
               (person_id, workType_id, name, price, caxs, note)
               VALUES(?,?,?,?,?,?) RETURNING id|]


updateQ :: Query
updateQ = [sql|
           UPDATE product 
           SET person_id=?, workType_id=?, name=?, price=?, caxs=?, note=?
           WHERE id=?
          |]     


personProductsQ :: Query
personProductsQ = [sql|
                      SELECT id, workType_id, name, price, caxs, note
                      FROM product 
                      WHERE person_id = ? |]   

