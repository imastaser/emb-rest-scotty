import Data.Yaml

readYamlFile :: FilePath -> IO Node
readYamlFile f =
  (maybe (error $ "Could not parse " ++ f) id) `fmap` decodeFile f