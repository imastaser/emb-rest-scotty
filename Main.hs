{-# LANGUAGE OverloadedStrings #-}


import Entity
import Web.Scotty
import Web.Scotty.Internal.Types 

import qualified Data.Aeson as A
--import Data.Monoid ((<>))
--import Prelude
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Internal.Lazy as L
import Database.PostgreSQL.Simple

import qualified Data.Text.Lazy as TL

import Network.HTTP.Types.Status (created201,
   internalServerError500, notFound404)



import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)


main :: IO ()
main = do
    -- TODO: get environemtn from command line
    argCfg <- parseArgConfig

    let env  = environment argCfg
    let logO = case env of
                Development -> logStdoutDev
                Testing     -> logStdoutDev
                Staging     -> logStdoutDev
                Production  -> logStdout
    cfg <- parseConfig env "postgresql.config"
    mkDB cfg -- creating db if not exists, do not need pool
    pool <- mkPool  cfg 
    migrate1 pool
    scotty (port argCfg) $ do
    middleware logO 

    get "/word/:word" $ wordR "word"
    get "/users" usersR
    post "/users" $ userP (jsonData :: ActionM User)
    get "/person/:id" $ do i  <- param "id"
                           [ps] <- liftIO $  findPerson pool i
                           json ps
    post "/person" $ do person <- getPersonParam -- read the request body, try to parse it into article
                        insertPerson pool person
                        status created201
                        json person     -- show info that the article was created

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
