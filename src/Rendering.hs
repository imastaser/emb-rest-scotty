{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Rendering where

import Control.Monad (foldM, mapM_)
import Data.Text.Internal (Text)
import Data.Text (unpack, pack)
import Data.Monoid ((<>))
import Entity
import Lucid
import Lucid.Base
import Lucid.Validation

css :: Text -> Html ()
css name = link_ [rel_ "stylesheet", type_ "text/css", href_ name]

js :: Text -> Html ()
js jsFile =(script_ [src_ jsFile] "")

jquery :: Html ()
jquery = do (js "/scripts/jquery/jquery-1.10.2.js")
            (js "/scripts/jquery/jquery-ui.js")
            (js "/scripts/jquery/jquery.to.json.js")
            (js "/scripts/jquery/jquery.validate.js")
            (js "/scripts/jquery/jquery.validate.unobtrusive.js")

commonjs :: Html ()
commonjs = do (js "/scripts/common/ajax.js")
              (js "/scripts/common/extensions.js")
              (js "/scripts/common/helpers.js")
              (js "/scripts/common/validation.extension.js")
         

personjs :: Html ()
personjs = js "/scripts/entity/person.js"

allCSS :: Html ()
allCSS = do (css "/css/styles.css")
            (css "http://fonts.googleapis.com/css?family=Karla:400,700,400italic,700italic")

allJS :: Html ()
allJS = do jquery
           commonjs


field :: Text -> Text -> Text -> Text -> Html ()
field name helpText inputType defaultValue =
  div_ $ do span_ (toHtml name)
            input_ [ type_ inputType
                   , name_ name
                   , id_  name
                   , value_ "Person Name"
                   , data_val_  "true"
                   , data_val_required_ "This field is required"
                   ]
            span_ (toHtml helpText)

renderPage :: Html () -> Html () -> Html ()
renderPage scripts body = 
            do head_ 
              $ do
                  allCSS
                  body_ $ body <> allJS <> scripts


renderAddPerson :: Html ()
renderAddPerson  =
  renderPage personjs $ do
    form_ [id_ "personForm", method_ "post"] $ do
      field "firstname" "" "text" ""
      field "lastname" "" "text" ""
      field "email" "" "text" ""
      br_ []
      input_ [type_ "button", id_ "addBtn", class_ "button", value_ "Add Person"]
