module Lucid.Validation where

import Lucid.Base
import Data.Text (Text)

-- | The @data_val@ attribute.
data_val_ :: Text -> Attribute
data_val_ = makeAttribute "data-val"  

-- | The @data_val@ attribute.
data_val_required_ :: Text -> Attribute
data_val_required_ = makeAttribute "data-val-required"  