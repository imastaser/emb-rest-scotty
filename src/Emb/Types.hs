{-# LANGUAGE OverloadedStrings #-}


module Emb.Types where

import qualified Data.Text  as T    

data Environment
   = Development
   | Production
   | Test
   deriving (Eq, Read, Show)




-- | DbConfig contains info needed to connect to PostgreSQL server
data DbConfig = DbConfig
             { dbHost        :: T.Text
             , dbPort        :: Int
             , dbName        :: T.Text
             , dbUser        :: T.Text
             , dbPassword    :: T.Text
             , dbConnections :: Int
             , dbStripes     :: Int -- - stripes, a single stripe is fine for many applications 
             , dbMaxAlive    :: Int -- - max keep alive (s)

             }   
    deriving (Show) 