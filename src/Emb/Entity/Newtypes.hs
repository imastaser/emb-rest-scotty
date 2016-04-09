{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Newtypes; foreign keys and such.

module Emb.Entity.Newtypes
       ( PersonId(..)
       , ProductId(..)
       , OrderId(..)
       ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

newtype PersonId = PersonId Int
  deriving (Eq,FromField,ToField,Ord)

instance Show PersonId where show (PersonId pid) = show pid

newtype ProductId = ProductId Int
  deriving (Integral,Real,Num,Ord,Eq,Enum,FromField,ToField)

instance Show ProductId where show (ProductId pid) = show pid

newtype OrderId = OrderId Int
  deriving (Integral,Real,Num,Ord,Eq,Enum,FromField,ToField)

instance Show OrderId where show (OrderId pid) = show pid

newtype LanguageId = LanguageId Int
  deriving (Integral,Real,Num,Ord,Eq,Enum,FromField,ToField)

instance Show LanguageId where show (LanguageId pid) = show pid

 