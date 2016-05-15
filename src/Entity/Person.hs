{-# OPTIONS -Wall #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | The product entity.

module Entity.Person
       ( Person(..)
       , insertPerson
       , updatePerson
       , deletePerson
       , findPerson
       , allPersons
       , createPerson -- for seeds
       )
       where

import DB.Dao
import Prelude hiding (id)
import Data.Pool(Pool, withResource)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

import Web.Scotty.Internal.Types (ActionT)


import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField



import Data.Aeson 
import GHC.Base hiding (id)
import GHC.Int(Int64)

import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Data.ByteString.Lazy.Char8 as BL

data Person = Person 
              {  personId     :: Int64
               , firstName    :: TL.Text
               , lastName     :: TL.Text
               , email        :: TL.Text
               , phone        :: TL.Text
               , phone2       :: TL.Text
               , note         :: TL.Text
              } 
              deriving (Show)

--instance FromRow Int64 where
--    fromRow = field

instance FromRow Person where
  fromRow = Person <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   
instance ToRow Person where
  toRow person =
    [ toField (firstName person)
    , toField (lastName person)
    , toField (email  person)
    , toField (phone  person)
    , toField (phone2 person)
    , toField (note person)
    ]

instance FromJSON Person where
  parseJSON (Object v) = 
    Person <$>
            v .:?  "id" .!= 0  <*> -- the field "id" is optional
            v .:   "firstname" <*>
            v .:   "lastname"  <*>
            v .:   "email"     <*>
            v .:   "phone"     <*>
            v .:   "phone2"    <*>
            v .:   "note"     

instance ToJSON Person where
     toJSON (Person _id _firstName _lastName _email _phone _phone2 _note) =
         object ["id"        .= _id,
                 "firstName" .= _firstName,
                 "lastName"  .= _lastName,
                 "email"     .= _email,
                 "phone"     .= _phone,
                 "phone2"    .= _phone2,
                 "note"      .= _note]   


insertPerson :: Pool Connection -> Maybe Person -> IO [Only Int64]
insertPerson _    Nothing = return [Only 0]
insertPerson pool (Just person) = fetch1 pool person insertQ 


createPerson :: Pool Connection -> Person -> IO [Only Int64]
createPerson pool person = fetch1 pool person insertQ 


findPerson :: Pool Connection -> Int -> IO [Person]
findPerson pool id = do
     res <- fetch1 pool (Only id) getPersonQ :: IO [Person] 
     return  res

allPersons :: Pool Connection -> IO [Person]
allPersons pool = do
     res <- fetchSimple1 pool allPersonQ :: IO [Person] 
     return  res


updatePerson :: Pool Connection -> Maybe Person -> Int64 -> ActionT TL.Text IO ()
updatePerson _ Nothing _ = return ()
updatePerson pool (Just (Person _ _firstName _lastName _email _phone _phone2 _note)) i = do
     _ <- liftIO $ execSqlT pool 
                       [_firstName, _lastName, _email, _phone, _phone2, _note, (TL.decodeUtf8 $ BL.pack $ show i)]
                       updateQ
     return ()     


deletePerson :: Pool Connection -> Int -> ActionT TL.Text IO ()
deletePerson pool _id = do
     _ <- liftIO $ execSqlT pool (Only _id) "DELETE FROM person WHERE id=?"
     return ()



-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------
allPersonQ :: Query
allPersonQ = [sql|
              SELECT id,firstname, lastname, email, phone, phone2, note 
              FROM person |]


insertQ :: Query
insertQ = [sql|
               INSERT INTO person
               (firstName, lastName, email, phone, phone2, note)
               VALUES(?,?,?,?,?,?) RETURNING id
          |]

updateQ :: Query
updateQ = [sql|
           UPDATE person 
           SET firstname=?, lastname=?, email=?, phone=?, phone2=?, note=?
           WHERE id=?
          |]


getPersonQ :: Query
getPersonQ = [sql|
              SELECT id,firstname, lastname, email, phone, phone2, note
              FROM person 
              WHERE id = ? |]


