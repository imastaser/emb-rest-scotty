 module Lucid.Customs where

import Lucid.Base
import Data.Text (Text)

-- | The @data_toggle@ attribute.
data_toggle_ :: Text -> Attribute
data_toggle_ = makeAttribute "data-toggle"  

-- | The @data_toggle@ attribute.
data_target_ :: Text -> Attribute
data_target_ = makeAttribute "data-target"

-- | The @data_method@ attribute.
data_method_ :: Text -> Attribute
data_method_ = makeAttribute "data-method"  

-- | The @data_confirm@ attribute.
data_confirm_ :: Text -> Attribute
data_confirm_ = makeAttribute "data-confirm" 

-- | The @data_tag@ attribute.
data_tag_ :: Text -> Attribute
data_tag_ = makeAttribute "data-tag"  


-- | The @aria_haspopup@ attribute.
aria_haspopup_ :: Text -> Attribute
aria_haspopup_ = makeAttribute "aria-haspopup" 

-- | The @aria_expanded@ attribute.
aria_expanded_ :: Text -> Attribute
aria_expanded_ = makeAttribute "aria-expanded" 