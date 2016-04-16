{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Rendering where

import Control.Monad (foldM, mapM_)
import Data.Text.Internal (Text)
import Data.Text (unpack, pack)
import Data.Monoid ((<>))
import Entity
import Lucid

import Lucid.Validation
import Lucid.Helper


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
            (css "/css/bootstrap.min.css")
            (css "http://fonts.googleapis.com/css?family=Karla:400,700,400italic,700italic")

allJS :: Html ()
allJS = do jquery
           commonjs


renderPage :: Html () -> Html () -> Html () -> Html ()
renderPage styles scripts body = 
      doctype_ <> html_
      (do head_ 
        $ do
            styles
            body_ $ do
              nav_ [class_ "navbar navbar-static-top"] navBar 
              div_ [class_ "container"] body
              scripts
       )


renderAddPerson :: Html ()
renderAddPerson  =
  renderPage allCSS (allJS <> personjs) $ do
    form_ [id_ "personForm", method_ "post"] $ do
      textBox "firstname" "Անուն" [  data_val_  "true"
                                  , data_val_required_ "This field is required"] 
      textBox "lastname"  "Ազգանուն" [] 
      textBox "email"  "" []
      br_ []
      input_ [type_ "button", id_ "addBtn", class_ "button", value_ "Add Person"]
