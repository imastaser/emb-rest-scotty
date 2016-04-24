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
          textBox  "Անուն" "firstname" "" reqAttr 
          textBox  "Ազգանուն" "lastname"  "" [] 
          textBox  "էլ-փոստ" "email"  "" []
          textBox  "բջջայիններ" "phone"  "" (reqAttr <> [style_ "display:inline;"])
          textBox  "" "phone2"  "" [style_ "display:inline;"]
          textArea "նշում" "note"  []
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
            th_ "Բջջային"
            th_ "Բջջային"
            th_ "նշում" 
        tbody_ $ do
           mapM_ (\p -> personRow p (personId p)) ps
      --with form_ [method_ "post", action_ "/", enctype_ "application/json"] $ do
      --  input_ [type_ "text", name_ "url"]
      --  with button_ [type_ "submit"] "Shorten"


personRow :: Person -> Int64 -> Html()
personRow p id = 
  tr_ [id_ newId] $ do
    td_ (toHtml $ firstName p)
    td_ (toHtml $ lastName p)
    td_ (toHtml $ email p)
    td_ (toHtml $ phone p)
    td_ (toHtml $ phone2 p)
    td_ (toHtml $ note p)
    td_ (a_ [ class_ "rest-edit btn btn-default"
            , href_ editUrl
            , data_method_ "put"
            , data_tag_ pid
            , href_ editUrl] "Խմբագրել")
    td_ (a_ [ class_ "rest-delete btn btn-danger"
            , data_method_ "delete"
            , data_confirm_ "Are you sure?"
            , data_tag_ pid
            , href_ deleteUrl
            , rel_ "nofollow"] "Ջնջել")
  where 
    pid :: Text
    pid = pack $ show id
    newId :: Text
    newId = pack $ "p_" ++ show id
    editUrl :: Text
    editUrl = pack $ "/person/" ++ show id ++ "edit"
    deleteUrl :: Text
    deleteUrl = pack $ "/person/" ++ show id
