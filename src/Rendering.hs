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
import Lucid.Customs
import GHC.Int(Int64)





renderAddPerson :: [Person] -> Html ()
renderAddPerson ps =
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
          input_ [type_ "button", id_ "addBtn", class_ "btn btn-default", value_ "Ավելացնել"]
        br_ []
        div_ [class_ "clear"] ""
        personsContent ps  

renderHomePage :: Html ()
renderHomePage  =
  renderPage mempty mempty "Embroidery Service Bookkeeping"

renderPersons :: [Person] -> Html()
renderPersons ps = 
  renderPage mempty mempty $
              personsContent ps


personsContent :: [Person] -> Html()
personsContent ps = 
    html_ $
      table_ [ id_ "ps", class_ "table table-hover"] $ do
        thead_ $
           tr_ $ do
            th_ "Անուն"
            th_ "Ազգանուն"
            th_ "email" 
        tbody_ $ do
           mapM_ (\p -> personRow p (personId p)) ps
      --with form_ [method_ "post", action_ "/", enctype_ "application/json"] $ do
      --  input_ [type_ "text", name_ "url"]
      --  with button_ [type_ "submit"] "Shorten"


personRow :: Person -> Int64 -> Html()
personRow p id = 
  tr_ [id_ newId] $ do
    td_ (toHtml $ personName p)
    td_ (toHtml $ personLastName p)
    td_ (toHtml $ personEmail p)
    td_ (a_ [class_ "btn btn-default", href_ editUrl] "Խմբագրել")
    td_ (a_ [ class_ "btn btn-danger"
            , data_method_ "delete"
            , data_confirm_ "Are you sure?"
            , href_ deleteUrl
            , rel_ "nofollow"] "Ջնջել")
  where 
    newId :: Text
    newId = pack $ "p_" ++ show id
    editUrl :: Text
    editUrl = pack $ "/person/" ++ show id ++ "edit"
    deleteUrl :: Text
    deleteUrl = pack $ "/person/" ++ show id
