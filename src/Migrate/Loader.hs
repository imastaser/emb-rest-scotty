module Migrate.Loader where


import System.Directory    


main = do
    d <- getCurrentDir 
    print d

getCurrentDir = getCurrentDirectory
setDir path = setCurrentDirectory path
filesInCurDir = getCurrentDirectory >>= getDirectoryContents