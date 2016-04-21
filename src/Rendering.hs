{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Rendering where

import Control.Monad (foldM, mapM_)
import Data.Text.Internal (Text)
import Data.Text (unpack, pack, append)
import Data.Monoid ((<>))
import Entity
import Lucid
import Lucid.Bootstrap
import Lucid.Validation
import Lucid.Helper
import GHC.Int(Int64)

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


renderAddPerson :: Html ()
renderAddPerson  =
  renderPage mempty personjs $ do
    row_ $ 
      span6_ $ do
        h2_ "Ավելացնել Հաճախորդ"
        br_ []        
        form_ [id_ "personForm", method_ "post"] $ do
          textBox "Անուն" "firstname" "" reqAttr 
          textBox "Ազգանուն" "lastname"  "" [] 
          textBox "էլ-փոստ" "email"  "" []
          br_ []
          input_ [type_ "button", id_ "addBtn", class_ "button", value_ "Ավելացնել"]


renderHomePage :: Html ()
renderHomePage  =
  renderPage mempty mempty "Embroidery Service Bookkeeping"

renderPersons :: [Person] -> Html()
renderPersons ps = 
  renderPage mempty mempty $
      html_ $
        body_ $ do
          h1_ "Title"
          p_ "Hello Lucid World!"
          table_ [ id_ "ps", class_ "table table-hover"] $ do
            thead_ $
               tr_ $ do
                th_ "Name"
                th_ "Last Name"
                th_ "Email" 
            tbody_ $ do
               mapM_ (\p -> personRow p (personId p)) ps
          with form_ [method_ "post", action_ "/", enctype_ "application/json"] $ do
            input_ [type_ "text", name_ "url"]
            with button_ [type_ "submit"] "Shorten"


personRow :: Person -> Int64 -> Html()
personRow p id = 
  tr_ [id_ newId] $ do
    td_ (toHtml $ personName p)
    td_ (toHtml $ personLastName p)
    td_ (toHtml $ personEmail p)
  where 
    newId :: Text
    newId = pack $ "p_" ++ show id
