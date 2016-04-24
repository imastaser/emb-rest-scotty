{-# LANGUAGE OverloadedStrings #-}

module Init.Config
    ( parseArgConfig
    ) where

import Init.Types
import qualified Data.Text as T
import System.Environment (getArgs, getProgName, getEnvironment)
import Data.Yaml
import Data.Maybe (fromMaybe)
import System.Exit (exitFailure)
import Data.Char (toUpper)

-- TODO: read config from yml files
-- http://lenguyenthedat.blogspot.am/2014/01/parsing-config-file-in-haskell-with-yaml.html
-- https://stackoverflow.com/questions/21292428/reading-yaml-lists-of-objects-in-haskell
-- http://blog.ssanj.net/posts/2014-10-09-How-to-read-a-YAML-file-from-Haskell.html
-- https://github.com/chrisdone/lpaste/blob/a7bb100480c666c21dab8957bfefd2ada7411ab3/src/Hpaste/Config.hs

data AppConfig environment = AppConfig
    { appEnv   :: environment
    , appPort  :: Int
    , appRoot  :: T.Text
    , appHost  :: T.Text
    } deriving (Show)  

-- https://github.com/yesodweb/yesod/blob/e7c6d06d3dca7710b6bad2caa074162170b82a78/yesod/Yesod/Default/Config.hs

parseArgConfig :: IO ArgConfig
parseArgConfig = do
    let envs = [minBound..maxBound] :: [DefaultEnv]
    args <- getArgs
    (portS, args') <- getPort id args
    portI <-
        case reads portS of
            (i, _):_ -> return i
            [] -> error $ "Invalid port value: " ++ show portS
    case args' of
        [e] -> do
            case reads $ capitalize e of
                (e', _):_ -> return $ ArgConfig e' portI
                [] -> do
                    () <- error $ "Invalid environment, valid entries are: " ++ show envs
                    -- next line just provided to force the type of envs
                    return $ ArgConfig (head envs) 0
        _ -> do
            pn <- getProgName
            putStrLn $ "Usage: " ++ pn ++ " <environment> [--port <port>]"
            putStrLn $ "Valid environments: " ++ show envs
            exitFailure
  where
    getPort front [] = do
        env <- getEnvironment
        return (fromMaybe "0" $ lookup "PORT" env, front [])
    getPort front ("--port":p:rest) = return (p, front rest)
    getPort front ("-p":p:rest) = return (p, front rest)
    getPort front (arg:rest) = getPort (front . (arg:)) rest

    capitalize [] = []
    capitalize (x:xs) = toUpper x : xs



--readYamlFile :: FilePath -> IO Node
--readYamlFile f =
--  (maybe (error $ "Could not parse " ++ f) id) `fmap` decodeFile f
   

