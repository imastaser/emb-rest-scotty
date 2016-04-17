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
import Lucid.Customs

metas :: Html()
metas = meta_ []


dropDownMenu :: Text -> Text -> [Menu] -> Html ()
dropDownMenu menuStyle name items = 
  with li_ [class_ menuStyle] $ do
    withA_ (do toHtml name; (span_ [class_ "caret"] ""))
    (ul_ [class_ "dropdown-menu"]   
       (mapM_ mkMenu_ items))
  where
    withA_ = with a_ [  href_ "#"  
                  , class_ "dropdown-toggle"
                  , data_toggle_ "dropdown" 
                  , role_ "button"
                  , aria_haspopup_ "true"
                  , aria_expanded_ "true"
                 ]

mkMenu_ :: Menu -> Html()
mkMenu_ (Menu txt url) = with li_ [] $ a_ [href_ url] (toHtml txt)
mkMenu_ (DropDown name ms) = dropDownMenu "dropdown" name ms
mkMenu_ (SubMenu name ms) = dropDownMenu "dropdown-submenu" name ms

navBar :: Html()
navBar = 
  with container_ [] $ do
   --(with a_ [ class_ "btn btn-navbar"
   --         , type_ "button"
   --         , data_toggle_ "collapse"
   --         , data_target_ ".nav-collapse"]  $ do
   --      span_ [class_ "icon-bar"] ""
   --      span_ [class_ "icon-bar"] ""
   --      span_ [class_ "icon-bar"] "")
   (with div_ [class_ "navbar-header"]  $ do
         a_ [class_ "navbar-brand", href_ "#"] "LiloArt")
   -- <>
   (with div_ [id_ "navbar", class_ "navbar-collapse collapse"] $ do
         ul_ [class_ "nav navbar-nav"] $ do
              (mapM_ mkMenu_  siteMenu))
              
              --(mapM_ (li_ . (a_ [href_ "#"]))  ["Home", "Menu"]))
              -- (dropDownMenu "Main" ["Item1", "item2"])

type Url = Text
data Menu = Menu Text Url | DropDown Text [Menu] | SubMenu Text [Menu]

siteMenu :: [Menu]
siteMenu = [  Menu "Home" "/"
            , DropDown "Person" 
                        [ Menu "All" "/person"
                        , Menu "Add" "/person/add"
                        , SubMenu "Menu" [Menu "Item1" "/", Menu "Item2" "/"]
                        ]
            , Menu "About" "/about"
           ]

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

field :: Text -> Text -> Text -> Text -> Text -> Text -> [Attribute] -> Html ()
field  inputType helpText defVal labelName name placeholder attrs =
  fieldset_ $ do label_ (toHtml labelName)
                 input_ ([ type_ inputType
                       , name_ name
                       , id_  name
                       , class_ "form-control"
                       , placeholder_ placeholder
                       , value_  defVal
                       ]<> attrs)
                 span_ (toHtml helpText)

textBox :: Text -> Text -> Text -> [Attribute] -> Html ()
textBox = field "text" "" "" 


reqAttr :: [Attribute]
reqAttr = [ data_val_  "true"
          , data_val_required_ "is required"]


