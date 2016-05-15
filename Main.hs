{-# LANGUAGE OverloadedStrings #-}

import Entity.Person
import Entity.User
import Entity.Product

import Views.About
import Views.Person
import Views.Product

import Init.Config (parseArgConfig)
import Init.Types

import DB.PostgreSQL (parseConfig, mkPool)
import DB.Migrate (migrate, mkDB)

import Web.Scotty
import Web.Scotty.Internal.Types 
import qualified Data.Aeson as A

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad (foldM, mapM_)

import Database.PostgreSQL.Simple
import Data.Foldable(forM_)
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Internal.Lazy as L
import Lucid
import Network.HTTP.Types.Status ( created201
                                 , internalServerError500
                                 , notFound404)

import GHC.Int(Int64)

import Data.Default (def)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger ( mkRequestLogger
                                            , logStdoutDev
                                            , logStdout
                                            , destination
                                            , RequestLoggerSettings(outputFormat)
                                            , OutputFormat(Detailed, Apache)
                                            , IPAddrSource(FromHeader, FromSocket))

lucidRender :: Html a -> ActionM ()
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
    migrate pool
    scotty (port argCfg) $ do
    
    middleware  logger -- logO 
    middleware $ staticPolicy (noDots >-> addBase "content")

    get  "/" $ do lucidRender  renderHomePage
    get "/about" $ do lucidRender renderAbout
    get "/users" usersR
    post "/users" $ userP (jsonData :: ActionM User)

    get "/person/:id" $ do i  <- param "id"
                           [ps] <- liftIO $  findPerson pool i
                           json ps
    
    get "/person/" $ do 
                       ps <- liftIO $  allPersons pool
                       lucidRender . renderPersons $ ps

                         
    get  "/person/add" $ do
                          ps <- liftIO $  allPersons pool 
                          html . renderText 
                            $ (renderAddPerson ps) 
    -- CREATE                        
    post "/person/add" $ do person <- getPersonParam
                            [Only newId] <- lift $ insertPerson pool person 
                            -- json person
                            case person of
                              (Just p) -> lucidRender $ personRow p newId
                              Nothing -> lucidRender $ p_"error"
    
    post "/person" $ do person <- getPersonParam
                        _ <- lift $ insertPerson pool person
                        -- status created201
                        json person     -- show info that the article was created
    
    -- UPDATE
    get "/person/:id/edit" $ do  i  <- param "id" 
                                 [p] <- liftIO $  findPerson pool i
                                 html . renderText $ (renderEditPerson i p)    
                            
    -- UPDATE
    put "/person/:id" $ do  i  <- param "id" :: ActionM Int64
                            person <- getPersonParam 
                            _ <- updatePerson pool person i
                            json person
     -- DELETE
    delete "/person/:id" 
          $ do pid <- param "id" -- :: ActionM TL.Text -- get the article id
               deletePerson pool pid  -- delete the article from the DB
               json ()      -- show info that the article was deleted


    -- Product CRUD

    -- index
    get "/person/:id/products" 
          $ do pid  <- param "id" 
               ps <- liftIO $  personProducts pool pid
               lucidRender . renderProducts $ ps

     -- CREATE - Add
    get  "/person/:id/product/add" 
          $ do pid  <- param "id"
               ps <- liftIO $  allProducts pool 
               html . renderText $ (renderAddProduct pid ps)   

    post "/person/:id/product/add" 
          $ do product <- getProductParam
               pid  <- param "id"
               [Only newId] <- lift $ insertProduct pool product pid
               case product of
                  (Just p) -> lucidRender $ productRow p newId
                  Nothing -> lucidRender $ p_"error: in insert product"  

    -- UPDATE - Edit
    get "/person/:id/product/:prod_id/edit" 
          $ do  prod_id  <- param "prod_id" 
                [p] <- liftIO $  findProduct pool prod_id
                html . renderText $ (renderEditProduct prod_id p)    
                            
    put "/person/:id/product/:prod_id" 
          $ do  prod_id  <- param "prod_id" :: ActionM Int64
                product <- getProductParam 
                _ <- updateProduct pool product prod_id
                json product


     -- DELETE 
    delete "/person/:id/product/:prod_id" 
          $ do prod_id <- param "prod_id" :: ActionM Int
               deleteProduct pool prod_id 
               json ()      
                                             

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


-- Parse the request body into the Product
getProductParam :: ActionT TL.Text IO (Maybe Product)
getProductParam = do b <- body
                     return $ (A.decode b :: Maybe Product)


