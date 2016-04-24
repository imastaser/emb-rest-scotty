{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

--{-# LANGUAGE  FlexibleContexts #-}


module Lucid.Helper where
import Lucid
import Lucid.Base
import Lucid.Bootstrap
import Lucid.Validation
import Data.Text.Internal (Text)
import Data.Monoid ((<>))
import Data.Text (unpack, pack, append)
import Data.String
import Lucid.Customs

metas :: Html()
metas = meta_ []


css :: Text -> Html ()
css name = link_ [rel_ "stylesheet", type_ "text/css", href_ name]

js :: Text -> Html ()
js script = (script_ [src_ script] "")

jquery :: Html ()
jquery = do (js "/scripts/jquery/jquery-1.10.2.js")
            (js "/scripts/jquery/jquery-ui.js")
            (js "/scripts/jquery/jquery.to.json.js")
            (js "/scripts/jquery/jquery.validate.js")
            (js "/scripts/jquery/jquery.validate.unobtrusive.js")
            (js "/scripts/bootstrap.min.js")

commonjs :: Html ()
commonjs = do (js "/scripts/common/ajax.js")
              (js "/scripts/common/extensions.js")
              (js "/scripts/common/helpers.js")
              (js "/scripts/common/validation.extension.js")
         

personjs :: Html ()
personjs = js "/scripts/entity/person.js"

allCSS :: Html ()
allCSS = do (css "/css/styles.css")
            (css "/css/bootstrap.css")
            (css "http://fonts.googleapis.com/css?family=Karla:400,700,400italic,700italic")

allJS :: Html ()
allJS = do jquery
           commonjs


renderPage :: Html () -> Html () -> Html () -> Html ()
renderPage styles scripts body = 
      doctype_ <> html_
      (do head_ 
        $ do
            (allCSS <> styles)
            body_ $ do
              nav_ [class_ "navbar navbar-static-top"] navBar 
              div_ [class_ "clear"] ""
              div_ [class_ "container"] body
              (allJS <> scripts)
       )



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
siteMenu = [  Menu "Գլխավոր" "/"
            , DropDown "Հաճախորդներ" 
                        [ Menu "Բոլորը" "/person"
                        , Menu "Ավելացնել" "/person/add"
                        -- , SubMenu "Menu" [Menu "Item1" "/", Menu "Item2" "/"]
                        ]
            , Menu "Մասին" "/about"
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

textArea :: Text -> Text -> [Attribute] -> Html ()
textArea labelName name attrs = 
        fieldset_ $ do label_ (toHtml labelName)
                       textarea_ ([ name_ name
                                   , id_  name
                                   , class_ "form-control"
                                   ]<> attrs) ""


reqAttr :: [Attribute]
reqAttr = [ data_val_  "true"
          , data_val_required_ "is required"]


