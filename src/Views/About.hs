{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Views.About where

import Data.Text.Internal (Text)
import Data.Text (unpack, pack, append)
import Lucid
import Lucid.Bootstrap
import Lucid.Helper



renderAbout :: Html ()
renderAbout =
  renderPage mempty mempty $ do
    row_ $ 
      span10_ $ do
        h2_ "About"
        br_ []        
        p_ "LiloArt 2007 "