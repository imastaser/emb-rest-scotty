{-# LANGUAGE OverloadedStrings #-}


module Emb.Types where

import qualified Data.Text  as T    

data Environment
   = Development
   | Production
   | Test
   deriving (Eq, Read, Show)




data DbConfig = DbConfig
             { dbHost        :: T.Text
             , dbPort        :: Int
             , dbName        :: T.Text
             , dbUser        :: T.Text
             , dbPassword    :: T.Text
             , dbConnections :: Int
             
             }   
    deriving (Show) 