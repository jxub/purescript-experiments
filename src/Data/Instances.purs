module Data.Instances where

import Prelude

data NonEmpty = NonEmpty a (Array a)

instance eqNonEmpty :: Eq NonEmpty