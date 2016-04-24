{-# OPTIONS -Wall #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | The product entity.

module Emb.Entity.Person
       ( Person(..)
       , insertPerson
       , updatePerson
       , deletePerson
       , findPerson
       , allPerson
       )
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
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField

import Emb.Entity.Newtypes
import Control.Applicative
import Data.Aeson 
import GHC.Base hiding (id)
import GHC.Int(Int64)

data Person = Person 
              {  personId       :: Int64
               , firstName     :: TL.Text
               , lastName :: TL.Text
               , email    :: TL.Text
               , phone    :: TL.Text
               , phone2   :: TL.Text
               , note   :: TL.Text
              } 
              deriving (Show)


----data Person = Person Integer TL.Text TL.Text TL.Text -- id title bodyText
    -- deriving (Show)              

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

instance FromRow PersonId where
    fromRow = field

--instance FromRow Id where
--    fromRow = field

instance FromRow Int where
    fromRow = field

instance FromRow Int64 where
    fromRow = field


instance ToJSON Person where
     toJSON (Person id firstName lastName email phone phone2 note) =
         object ["id" .= id,
                 "firstName" .= firstName,
                 "lastName" .= lastName,
                 "email"  .= email,
                 "phone"  .= phone,
                 "phone2"  .= phone2,
                 "note"  .= note]   


insertPerson :: Pool Connection -> Maybe Person -> IO [Only Int64]
insertPerson _    Nothing = return [Only 0]
insertPerson pool (Just person) = fetch1 pool person insertPersonQ 


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
updatePerson pool (Just (Person id firstname lastname email phone phone2 note)) = do
     _ <- liftIO $ execSqlT pool 
                       [firstname, lastname, email, phone, phone2, note, (TL.decodeUtf8 $ BL.pack $ show id)]
                       updatePersonQ
     return ()     


deletePerson :: Pool Connection -> Int -> ActionT TL.Text IO ()
deletePerson pool id = do
     liftIO $ execSqlT pool (Only id) "DELETE FROM person WHERE id=?"
     return ()

  --deletePerson1 :: Pool Connection -> Int -> ActionT TL.Text IO ()
  --deletePerson1 pool id = runDb pool $ \c -> do
  --        void $ execute c [sql|DELETE FROM person WHERE id=?|] [Only id]
  --        return ()     
  --     where
  --      -- runDb :: Pool Connection -> (Connection -> IO a) -> IO a
  --      runDb pool f =
  --        withResource pool (\conn -> withTransaction conn (f conn))


-------------------------------------------------------------------------------
-- Queries
-------------------------------------------------------------------------------
allPersonQ :: Query
allPersonQ = [sql|
              SELECT id,firstname, lastname, email, phone, phone2, note 
              FROM person |]


insertPersonQ :: Query
insertPersonQ = [sql|
                 INSERT INTO person
                 (firstName, lastName, email, phone, phone2, note)
                 VALUES(?,?,?,?,?,?) RETURNING id|]

updatePersonQ :: Query
updatePersonQ = [sql|
                 UPDATE person 
                 SET firstname=?, lastname=?, email=?, phone=?, phone2=?, note=?
                 WHERE id=?
                |]


getPersonQ :: Query
getPersonQ = [sql|
              SELECT id,firstname, lastname, email, phone, phone2, note
              FROM person 
              WHERE id = ? |]


