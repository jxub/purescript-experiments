module Lens.Optics where

import Prelude

-- import Product
import Data.Lens (lens, view, set, over, _1, _2)
import Data.Tuple
import Data.String as String

_first =
    lens getter setter
    where
        getter = fst
        setter Tuple (_ kept) new =
            Tuple new kept
