 module Lucid.Customs where

import Lucid.Base
import Data.Text (Text)

-- | The @data_toggle@ attribute.
data_toggle_ :: Text -> Attribute
data_toggle_ = makeAttribute "data-toggle"  

-- | The @data_toggle@ attribute.
data_target_ :: Text -> Attribute
data_target_ = makeAttribute "data-target"  


-- | The @aria_haspopup@ attribute.
aria_haspopup_ :: Text -> Attribute
aria_haspopup_ = makeAttribute "aria-haspopup" 

-- | The @aria_expanded@ attribute.
aria_expanded_ :: Text -> Attribute
aria_expanded_ = makeAttribute "aria-expanded" 