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


footer :: Html ()
footer =
  html_ $ do
      footer_ $ do
        p_ $ small_ "LiloArt © 2007 "


-- | The @renderPage@ make html layout.
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
              footer  
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

-- | The @mkMenu_@ function.
mkMenu_ :: Menu -> Html()
mkMenu_ (Menu txt url) = with li_ [] $ a_ [href_ url] (toHtml txt)
mkMenu_ (DropDown name ms) = dropDownMenu "dropdown" name ms
mkMenu_ (SubMenu name ms) = dropDownMenu "dropdown-submenu" name ms

-- | The @navBar@ html element.
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

-- | The @Url@ type.
type Url = Text

-- | The @Menu@ recursive data.
data Menu = Menu Text Url | DropDown Text [Menu] | SubMenu Text [Menu]

-- | The @siteMenu@ structure.
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

-- | The @field@ form element.
field :: Text -> Text -> Text -> Text -> Text -> Text -> [Attribute] -> Html ()
field  inputType helpText labelName name defVal placeholder attrs =
  fieldset_ $ do label_ (toHtml labelName)
                 input_ ([ type_ inputType
                       , name_ name
                       , id_  name
                       , class_ "form-control"
                       , placeholder_ placeholder
                       , value_  defVal
                       ]<> attrs)
                 span_ (toHtml helpText)

-- | The @textBox@ element.
textBox :: Text -> Text -> Text -> Text -> [Attribute] -> Html ()
textBox = field "text" ""  

-- | The @textArea@ element.
textArea :: Text -> Text -> [Attribute] -> Html ()
textArea labelName name attrs = 
        fieldset_ $ do label_ (toHtml labelName)
                       textarea_ ([ name_ name
                                   , id_  name
                                   , class_ "form-control"
                                   ]<> attrs) ""

-- | The @reqAttr@ attribute.
reqAttr :: [Attribute]
reqAttr = [ data_val_  "true"
          , data_val_required_ "is required"]


