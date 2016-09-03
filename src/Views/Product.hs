{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Views.Product where

import Control.Monad (foldM, mapM_)
import Data.Text.Internal  as T
import Data.Text (unpack, pack, append)
import Data.Monoid ((<>))
import Entity.Product
import Lucid
import Lucid.Bootstrap
import Lucid.Validation
import Lucid.Helper
import Lucid.Customs
import GHC.Int(Int64)

import qualified Data.Text.Lazy as TL

renderProducts :: [Product] -> Html()
renderProducts ps = 
  renderPage mempty mempty $
              productsTable ps



renderAddProduct :: Int64 -> [Product] -> Html ()
renderAddProduct i ps =
  renderPage mempty productjs $ do
    row_ $ 
      span6_ $ do
        h2_ "Ավելացնել Product"
        br_ []        
        form_ [id_ "productForm", method_ "post", tag_ (pack $ show i)] $ do
          numberBox  "Type" "workType_id" "" "" reqAttr 
          textBox  "Անուն" "name"  "" "" [] 
          numberBox  "price" "price"  "" "" []
          numberBox  "caxs" "caxs"  "" "" []
          textArea "նշում" "note"  []
          br_ []
          input_ [type_ "button", id_ "addProductBtn", class_ "btn btn-default", value_ "Ավելացնել"]
        br_ []
        div_ [class_ "clear"] ""
        productsTable ps 

productsTable :: [Product] -> Html()
productsTable ps = 
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
           mapM_ (\p -> productRow p (productId p)) ps


-- | the row to return ajax after insert
productRow :: Product -> Int64 -> Html()
productRow p id = 
  tr_ [id_ newId] $ do
    td_ (toHtml $ show $ workType_Id p)
    td_ (toHtml $ productName p)
    td_ (toHtml $ show $ productPrice p)
    td_ (toHtml $ show $ caxs p)
    td_ (toHtml $ productNote p)
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
    pid = pack $ show (person_Id p)
    newId :: T.Text
    newId = pack $ "p_" ++ show (person_Id p)
    editUrl :: T.Text
    editUrl = pack $ "/person/" ++ show (person_Id p) ++ "/product/" ++ show id ++ "/edit"
    deleteUrl :: T.Text
    deleteUrl = pack $ "/person/" ++ show (person_Id p) ++ "/product/" ++ show id           



renderEditProduct :: Int -> Product -> Html ()
renderEditProduct id p =
  renderPage mempty productjs $ do
    row_ $ 
      span6_ $ do
        h2_ "Xmbagrel product"
        br_ []
        form_ [id_ "productForm", method_ "put", action_ actionUrl, tag_ (pack $ show id)] $ do
          numberBox  "Type" "workType_id"  (pack . show $ workType_Id p) "" reqAttr 
          textBox  "անուն" "name"  (TL.toStrict $ productName p) "" [] 
          numberBox  "price" "price"  (pack . show $ productPrice p) "" []
          numberBox  "caxs" "caxs"  (pack . show $ caxs p) "" (reqAttr <> [style_ "display:inline;"])
          textArea "նշում" "note"  []
          br_ []
          input_ [type_ "button", id_ "saveBtn", class_ "btn btn-default", value_ "Հիշել"]
        br_ []
        div_ [class_ "clear"] ""
   where
      actionUrl :: T.Text
      actionUrl = pack $ "/person/" ++ show (person_Id p) ++ "/product/"++ show id
