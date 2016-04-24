{-# OPTIONS -Wall #-}

-- | The product entity.

module Entity.Product
       (Product(..))
       where

import Emb.Entity.Newtypes
import Control.Applicative
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Aeson 

data Product = Product
              {
                 productId         :: ProductId
               , productPersonId   :: PersonId
               , productName       :: Text
               , productPrice      :: Int
              } 
              deriving (Show)

instance FromRow Product where
  fromRow = Product <$> field
                    <*> field
                    <*> field
                    <*> field

instance FromRow ProductId where
    fromRow = field

instance FromRow PersonId where
    fromRow = field    

instance FromRow Int where
    fromRow = field


{-- postProduct :: (HasPostgres m) => T.Text -> Int -> PersonId -> m Int
postProduct name price person = do
    productids <- returning insertQ [(name, price, person)]
    return $ head productids                    
--}
insertQ :: Query
insertQ = "INSERT INTO product (name,price,person_id) VALUES (?, ?, ?) RETURNING id" 