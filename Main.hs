{-# LANGUAGE OverloadedStrings #-}

import Entity
import Views
import Web.Scotty
import Web.Scotty.Internal.Types 

import Rendering 

import qualified Data.Aeson as A
import Emb.Entity.Newtypes (PersonId(..))
--import Data.Monoid ((<>))
--import Prelude
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad (foldM, mapM_)
import qualified Data.Text.Internal.Lazy as L
import Database.PostgreSQL.Simple
import Data.Foldable(forM_)
import qualified Data.Text.Lazy as TL
import Lucid
import Data.Monoid ((<>))
import Network.HTTP.Types.Status ( created201
                                 , internalServerError500
                                 , notFound404)


import Data.Default (def)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger ( mkRequestLogger
                                            , logStdoutDev
                                            , logStdout
                                            , destination
                                            , RequestLoggerSettings(outputFormat)
                                            , OutputFormat(Detailed, Apache)
                                            , IPAddrSource(FromHeader, FromSocket))

import Data.Text.Lazy (toStrict)

lucidRender = html . renderText

main :: IO ()
main = do
    -- TODO: get environemtn from command line
    argCfg <- parseArgConfig

    let env  = environment argCfg
    let logO = case env of
                Development -> logStdoutDev
                Testing     -> logStdoutDev
                Staging     -> logStdoutDev --  Detailed True logging format and logs to stdout
                Production  -> logStdout    --  This uses the Apache logging format
    cfg <- parseConfig env "postgresql.config"
    mkDB cfg -- creating db if not exists, do not need pool
    logger <-  mkRequestLogger def 
                       { outputFormat =
                              case env of
                                Development -> Detailed True
                                _           -> Apache FromSocket
                                -- if nginx should Apache FromHeader
                       -- , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation  
                       }  
    pool <- mkPool  cfg 
    migrate1 pool
    scotty (port argCfg) $ do
    
    middleware  logger -- logO 
    middleware $ staticPolicy (noDots >-> addBase "content")

    get  "/" $ do lucidRender  renderHomePage
    get "/about" $ do lucidRender renderAbout
    get "/word/:word" $ wordR "word"
    get "/users" usersR
    post "/users" $ userP (jsonData :: ActionM User)

    get "/person/:id" $ do i  <- param "id"
                           [ps] <- liftIO $  findPerson pool i
                           json ps
    get "/person/" $ do 
                       ps <- liftIO $  allPerson pool
                       lucidRender . renderPersons $ ps
                         
    get  "/person/add" $ do
                          ps <- liftIO $  allPerson pool 
                          html . renderText 
                            $ (renderAddPerson ps) 

    post "/person/add" $ do person <- getPersonParam
                            [Only newId] <- lift $ insertPerson pool person 
                            -- json person
                            case person of
                              (Just p) -> lucidRender $ personRow p newId
                              Nothing -> lucidRender $ p_"error"
    post "/person" $ do person <- getPersonParam
                        lift $ insertPerson pool person
                        -- status created201
                        json person     -- show info that the article was created
    
    put "/person" $ do  person <- getPersonParam 
                        updatePerson pool person
                        json person     
     -- DELETE
    delete "/person/:id" $ do id <- param "id" :: ActionM TL.Text -- get the article id
                              deletePerson pool id  -- delete the article from the DB
                              json id      -- show info that the article was deleted

    -- get "/testpg" $ text ( testPg)
    -- wordR :: Data.Text.Internal.Lazy.Text -> ActionM ()
    -- wordR :: L.Text -> ActionM ()

wordR ::L.Text -> ActionT L.Text IO ()
wordR w = do
    word <- param w
    html $ mconcat ["<h1>Scotty, ", word, " me up!</h1>"]

usersR :: ActionM ()
usersR = do
    json allUsers

userP :: ActionM User -> ActionM()
userP jsonUser = do
    user <- jsonUser
    json user

-- Parse the request body into the Person
getPersonParam :: ActionT TL.Text IO (Maybe Person)
getPersonParam = do b <- body
                    return $ (A.decode b :: Maybe Person)

