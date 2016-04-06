{-# LANGUAGE OverloadedStrings #-}


import Entity
import Web.Scotty
import Web.Scotty.Internal.Types 
--import Data.Monoid ((<>))
--import Prelude
    
import qualified Data.Text.Internal.Lazy as L



main :: IO ()
main = scotty 3131 $ do
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
