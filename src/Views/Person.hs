{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Views.Person where

import Control.Monad (foldM, mapM_)
import Data.Text.Internal  as T
import Data.Text (unpack, pack, append)
import Data.Monoid ((<>))
import Entity.Person
import Lucid
import Lucid.Bootstrap
import Lucid.Validation
import Lucid.Helper
import Lucid.Customs
import GHC.Int(Int64)
import qualified Data.Text.Lazy             as TL
import qualified Data.ByteString.Lazy.Char8 as BL


_editForm :: Int -> TL.Text -> TL.Text -> TL.Text -> TL.Text -> TL.Text -> Html() 
_editForm id firstName lastName email phone phone2 = 
        form_ [id_ "personForm", method_ "put", action_ actionUrl, tag_ (pack $ show id)] $ do
          textBox  "Անուն" "firstname"  (TL.toStrict firstName) "" reqAttr 
          textBox  "Ազգանուն" "lastname"  (TL.toStrict lastName) "" [] 
          textBox  "էլ-փոստ" "email"  (TL.toStrict email) "" []
          textBox  "բջջայիններ" "phone"  (TL.toStrict phone) "" (reqAttr <> [style_ "display:inline;"])
          textBox  "" "phone2"  (TL.toStrict phone2) "" [style_ "display:inline;"]
          textArea "նշում" "note"  []
          br_ []
          input_ [type_ "button", id_ "saveBtn", class_ "btn btn-default", value_ "Հիշել"]
    where
      actionUrl :: T.Text
      actionUrl = pack $ "/person/" ++ show id


renderEditPerson :: Int -> Person -> Html ()
renderEditPerson id p =
  renderPage mempty personjs $ do
    row_ $ 
      span6_ $ do
        h2_ "Ավելացնել Հաճախորդ"
        br_ []
        _editForm id (firstName p) (lastName p) (email p) (phone p) (phone2 p)      
        br_ []
        div_ [class_ "clear"] ""
   


renderAddPerson :: [Person] -> Html ()
renderAddPerson ps =
  renderPage mempty personjs $ do
    row_ $ 
      span6_ $ do
        h2_ "Ավելացնել Հաճախորդ"
        br_ []        
        form_ [id_ "personForm", method_ "post"] $ do
          textBox  "Անուն" "firstname" "" "" reqAttr 
          textBox  "Ազգանուն" "lastname"  "" "" [] 
          textBox  "էլ-փոստ" "email"  "" "" []
          textBox  "բջջայիններ" "phone"  "" "" (reqAttr <> [style_ "display:inline;"])
          textBox  "" "phone2"  "" "" [style_ "display:inline;"]
          textArea "նշում" "note"  []
          br_ []
          input_ [type_ "button", id_ "addBtn", class_ "btn btn-default", value_ "Ավելացնել"]
        br_ []
        div_ [class_ "clear"] ""
        personsTable ps  

renderHomePage :: Html ()
renderHomePage  =
  renderPage mempty mempty "Embroidery Service Bookkeeping"

renderPersons :: [Person] -> Html()
renderPersons ps = 
  renderPage mempty mempty $
              personsTable ps


personsTable :: [Person] -> Html()
personsTable ps = 
    html_ $
      table_ [ id_ "ps", class_ "table table-hover"] $ do
        thead_ $
           tr_ $ do
            th_ "Անուն"
            th_ "Ազգանուն"
            th_ "էլ-փոստ" 
            th_ "Բջջային"
            th_ "Բջջային"
            th_ "նշում" 
        tbody_ $ do
           mapM_ (\p -> personRow p (personId p)) ps
      --with form_ [method_ "post", action_ "/", enctype_ "application/json"] $ do
      --  input_ [type_ "text", name_ "url"]
      --  with button_ [type_ "submit"] "Shorten"

-- | the row to return ajax after insert
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
            , data_tag_ pid
            , href_ editUrl] "Խմբագրել")
    td_ (a_ [ class_ "rest-delete btn btn-danger"
            , data_method_ "delete"
            , data_confirm_ "Are you sure?"
            , data_tag_ pid
            , href_ deleteUrl
            , rel_ "nofollow"] "Ջնջել")
  where 
    pid :: T.Text
    pid = pack $ show id
    newId :: T.Text
    newId = pack $ "p_" ++ show id
    editUrl :: T.Text
    editUrl = pack $ "/person/" ++ show id ++ "/edit"
    deleteUrl :: T.Text
    deleteUrl = pack $ "/person/" ++ show id
