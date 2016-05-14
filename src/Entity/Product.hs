{-# OPTIONS -Wall #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | The product entity.

module Entity.Product
       (Product(..))
       where

import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Data.ByteString.Lazy.Char8 as BL

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.FromRow


import Data.Aeson 
import GHC.Int(Int64) 

data Product = Product
              {
                 productId         :: Int64
               , person_Id         :: Int64
               , workType_Id       :: Int64
               , productName       :: TL.Text
               , productPrice      :: Float
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


{-- postProduct :: (HasPostgres m) => T.Text -> Int -> PersonId -> m Int
postProduct name price person = do
    productids <- returning insertQ [(name, price, person)]
    return $ head productids                    
--}


-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------

insertQ :: Query
insertQ = [sql|INSERT INTO product
               (person_id, workType_id, name, price, note)
               VALUES(?,?,?,?,?) RETURNING id|]


updateQ :: Query
updateQ = [sql|
           UPDATE product 
           SET person_id=?, workType_id=?, name=?, price=?, note=?
           WHERE id=?
          |]               