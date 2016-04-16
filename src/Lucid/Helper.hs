{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE  FlexibleContexts #-}


module Lucid.Helper where
import Lucid
import Lucid.Base
import Lucid.Bootstrap
import Lucid.Validation
import Data.Text.Internal (Text)
import Data.Monoid ((<>))
import Data.String


navBar :: Html()
navBar = 
  with container_ [] $
   (with div_ [class_ "navbar-header"]  $ do
         a_ [class_ "navbar-brand", href_ "#"] "LiloArt")
   <>
   (with div_ [id_ "navbar", class_ "navbar-collapse collapse"] $ do
         ul_ [class_ "nav navbar-nav"] 
              (mapM_ (li_ . (a_ [href_ "#"]))  ["Home", "Menu"]))

--page1 =
--  html_ (do head_ (do title_ "Introduction page."
--                      with link_
--                           [rel_ "stylesheet"
--                           ,type_ "text/css"
--                           ,href_ "screen.css"])
--            body_ (do with div_ [id_ "header"] "Syntax"
--                      p_ "This is an example of Lucid syntax."
--                      ul_ (mapM_ (li_ . toHtml . show)
--                                 [1,2,3])))

field :: Text -> Text -> Text -> Text -> Text -> [Attribute] -> Html ()
field  inputType helpText defVal name placeholder attrs =
  fieldset_ $ do label_ (toHtml name)
                 input_ ([ type_ inputType
                       , name_ name
                       , id_  name
                       , class_ "form-control"
                       , placeholder_ placeholder
                       , value_  defVal
                       ]<> attrs)
                 span_ (toHtml helpText)

textBox :: Text -> Text -> [Attribute] -> Html ()
textBox = field "text" "" "" 




