{-# OPTIONS -Wall #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | The product entity.

module Emb.Entity.Person
       ( Person(..)
       , insertPerson
       , updatePerson
       , deletePerson
       , findPerson
       , allPerson)
       where
import DB.Dao
import Data.Pool(Pool, withResource)
import Control.Monad.IO.Class (liftIO)

import Control.Monad (void)
import Web.Scotty.Internal.Types (ActionT)
import qualified Data.Text.Lazy as TL

import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Char8 as BL


import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Emb.Entity.Newtypes
import Control.Applicative

import Database.PostgreSQL.Simple.FromRow
import Data.Aeson 
import GHC.Base hiding (id)
{--data Person = Person 
              {  personId       :: PersonId
               , personName     :: Text
               , personLastName :: Text
               , personEmail    :: Text
              } 
              deriving (Show)
--}
data Person = Person Integer TL.Text TL.Text TL.Text -- id title bodyText
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

instance FromRow PersonId where
    fromRow = field

instance FromRow Int where
    fromRow = field


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


findPerson :: Pool Connection -> Int -> IO [Person]
findPerson pool id = do
     res <- fetch1 pool (Only id) getPersonQ :: IO [Person] 
     return  res

allPerson :: Pool Connection -> IO [Person]
allPerson pool = do
     res <- fetchSimple1 pool allPersonQ :: IO [Person] 
     return  res


updatePerson :: Pool Connection -> Maybe Person -> ActionT TL.Text IO ()
updatePerson _ Nothing = return ()
updatePerson pool (Just (Person id firstname lastname email)) = do
     _ <- liftIO $ execSqlT pool 
                       [firstname, lastname, email, (TL.decodeUtf8 $ BL.pack $ show id)]
                       updatePersonQ
     return ()     


deletePerson :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
deletePerson pool id = do
     liftIO $ execSqlT pool [id] "DELETE FROM person WHERE id=?"
     return ()
-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------
allPersonQ :: Query
allPersonQ = [sql|
              SELECT id,firstname, lastname, email 
              FROM person |]


insertPersonQ :: Query
insertPersonQ = [sql|
                 INSERT INTO person
                 (firstName, lastName, email)
                 VALUES(?,?,?)|]

updatePersonQ :: Query
updatePersonQ = [sql|
                 UPDATE person 
                 SET firstname=?, lastname=?, email=?
                 WHERE id=?
                |]


getPersonQ :: Query
getPersonQ = [sql|
              SELECT id,firstname, lastname, email 
              FROM person 
              WHERE id = ? |]


