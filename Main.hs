{-# LANGUAGE OverloadedStrings #-}


import Entity
import Web.Scotty
import Web.Scotty.Internal.Types 


--import Data.Monoid ((<>))
--import Prelude
    
import qualified Data.Text.Internal.Lazy as L
import Database.PostgreSQL.Simple

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
