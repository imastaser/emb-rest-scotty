{-# OPTIONS -Wall #-}

-- | The product entity.

module Emb.Entity.Person
       ( Person(..)
       , insertPerson
       , findPerson)
       where
import DB.Dao
import Data.Pool(Pool)
import Control.Monad.IO.Class (liftIO)

import Web.Scotty.Internal.Types (ActionT)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL


import Emb.Entity.Newtypes
import Control.Applicative
import Data.Text                               (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Aeson (FromJSON, ToJSON) 
import GHC.Base hiding (id)
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
  parseJSON (Object v) = 
    Person <$>
            v .:? "id" .!= 0  <*> -- the field "id" is optional
            v .:  "firstName" <*>
            v .:  "lastName"  <*>
            v .:  "email"    

instance ToJSON Person where
     toJSON (Person id firstName lastName email) =
         object ["id" .= id,
                 "firstName" .= firstName,
                 "lastName" .= lastName,
                 "email"  .= email]   


insertPerson :: Pool Connection -> Maybe Person -> ActionT TL.Text IO ()
insertPerson _    Nothing = return ()
insertPerson pool (Just (Person _ firstName lastName email)) = do
     _ <- liftIO $ execSqlT pool [firstName, lastName, email] 
                            insertPersonQ
     return ()

insertPersonQ :: Query
insertPersonQ = "INSERT INTO person(firstName, lastName, email) VALUES(?,?,?)"

findPerson :: Pool Connection -> Int -> IO [Person]
findPerson pool id = do
     res <- fetch1 pool (Only id) getPersonQ :: IO [Person] 
     return  res

getPersonQ :: Query
getPersonQ = "Select id,firstname, lastname, email FROM person WHERE id = ?"


