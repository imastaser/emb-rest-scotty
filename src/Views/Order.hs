{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Views.Order where

import Control.Monad (foldM, mapM_)
import Data.Text.Internal  as T
import Data.Text (unpack, pack, append)
import Data.Monoid ((<>))
import Entity.Order
import Lucid
import Lucid.Bootstrap
import Lucid.Validation
import Lucid.Helper
import Lucid.Customs
import GHC.Int(Int64)

import qualified Data.Text.Lazy as TL

renderOrders :: [Order] -> Html()
renderOrders ps = 
  renderPage mempty mempty $
              ordersTable ps


renderAddOrder :: Int64 -> [Order] -> Html ()
renderAddOrder i ps =
  renderPage mempty orderjs $ do
    row_ $ 
      span6_ $ do
        h2_ "Ավելացնել order"
        br_ []        
        form_ [id_ "orderForm", method_ "post", tag_ (pack $ show i)] $ do
          numberBox  "Ապրանք" "product_id"  "" "" [] 
          numberBox  "Գին" "price"  "" "" []
          numberBox  "Ծախս" "caxs"  "" "" []
          numberBox  "Քանակ" "caxs"  "" "" []
          textBox  "Ամսաթիվ" "datetime"  "" "" []
          textArea "նշում" "note"  []
          br_ []
          input_ [type_ "button", id_ "addOrderBtn", class_ "btn btn-default", value_ "Ավելացնել"]
        br_ []
        div_ [class_ "clear"] ""
        select_ [id_ "personCmb"] ""
        ordersTable ps 


ordersTable :: [Order] -> Html()
ordersTable ps = 
      table_ [ id_ "ps", class_ "table table-hover"] $ do
        thead_ $
           tr_ $ do
            th_ "Ապրանք"
            th_ "Գին"
            th_ "Ծախս" 
            th_ "Քանակ"
            th_ "Ամսաթիվ"
            th_ "նշում" 
        tbody_ $ do
           mapM_ (\p -> orderRow p (orderId p)) ps


-- | the row to return ajax after insert
orderRow :: Order -> Int64 -> Html()
orderRow p id = 
  tr_ [id_ newId] $ do
    td_ (toHtml $ show $ product_Id p)
    td_ (toHtml $ show $ price p)
    td_ (toHtml $ show $ caxs p)
    td_ (toHtml $ show $ count p)
    td_ (toHtml $ show $ datetime p)
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
    pid = pack $ show (person_Id p)
    newId :: T.Text
    newId = pack $ "p_" ++ show (person_Id p)
    editUrl :: T.Text
    editUrl = pack $ "/person/" ++ show (person_Id p) ++ "/order/" ++ show id ++ "/edit"
    deleteUrl :: T.Text
    deleteUrl = pack $ "/person/" ++ show (person_Id p) ++ "/order/" ++ show id           



renderEditOrder :: Int -> Order -> Html ()
renderEditOrder id p =
  renderPage mempty productjs $ do
    row_ $ 
      span6_ $ do
        h2_ "Xmbagrel order"
        br_ []
        form_ [id_ "orderForm", method_ "put", action_ actionUrl, tag_ (pack $ show id)] $ do
          numberBox  "Ապրանք" "product_id"  (pack . show $ product_Id p) "" reqAttr 
          textBox  "Գին" "price"  (pack . show $ price p) "" [] 
          numberBox  "Ծախս" "caxs"  (pack . show $ caxs p) "" []
          numberBox  "Քանակ" "count"  (pack . show $ count p) "" []
          textArea "Ամսաթիվ" "datetime"  []
          textArea "նշում" "note"  []
          br_ []
          input_ [type_ "button", id_ "saveBtn", class_ "btn btn-default", value_ "Հիշել"]
        br_ []
        div_ [class_ "clear"] ""
   where
      actionUrl :: T.Text
      actionUrl = pack $ "/person/" ++ show (person_Id p) ++ "/order/"++ show id
