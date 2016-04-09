{-# OPTIONS -Wall #-}

-- | The product entity.

module Emb.Entity.Person
       (Person(..))
       where

import Emb.Entity.Newtypes
import Control.Applicative
import Data.Text                               (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Aeson

{--data Person = Person 
              {  personId       :: PersonId
               , personName     :: Text
               , personLastName :: Text
               , personEmail    :: Text
              } 
              deriving (Show) --}

data Person = Person Integer Text Text Text -- id title bodyText
     deriving (Show)              

instance FromRow Person where
  fromRow = Person <$> field
                   <*> field
                   <*> field
                   <*> field

instance FromJSON Person where
     parseJSON (Object v) = Person <$>
                            v .:? "id" .!= 0 <*> -- the field "id" is optional
                            v .:  "firstName"    <*>
                            v .:  "lastName"     <*>
                            v .:  "email"    

instance ToJSON Person where
     toJSON (Person id firstName lastName email) =
         object ["id" .= id,
                 "firstName" .= firstName,
                 "lastName" .= lastName,
                 "email"  .= email]                   
