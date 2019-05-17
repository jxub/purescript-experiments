module Data.Arrays where

import Prelude ((<$>), (>=))

import Data.Array (filter)
import Data.Foldable (foldl, foldr)
import Data.HeytingAlgebra ((&&))
import Math (sqrt)

squaresArray :: Array Number -> Array Number
squaresArray arr = sqrt <$> arr

removeNegatives :: Array Int -> Array Int
removeNegatives arr = filter (\n -> n >= 0) arr

areTrue :: Array Boolean -> Boolean
areTrue arr = foldl (&&) true arr

-- reverse :: Array Number -> Array Number
reverse arr = foldl (\a b -> b a) [] arr