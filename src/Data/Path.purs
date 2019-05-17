module Data.Path where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array (concatMap, (:), filter, catMaybes)
import Data.Foldable (foldl)

allFiles :: Path -> Array Path
allFiles file = file : concatMap allFiles (ls file)

allFiles' :: Path -> Array Path
allFiles' file = file : do
    child <- ls file
    allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles (File name size) = pure $ File name size
onlyFiles (Directory name files) = concatMap onlyFiles files

largestFileSize :: Path -> Int
largestFileSize (File name size) = size
largestFileSize (Directory name size') = foldl max 0 $
    catMaybes $
    map size $
    onlyFiles (Directory name size')

data Path
    = Directory String (Array Path)
    | File String Int

instance showPath :: Show Path where
    show = filename

filename :: Path -> String
filename (File      name _) = name
filename (Directory name _) = name

isDirectory :: Path -> Boolean
isDirectory (File      _ _) = false
isDirectory (Directory _ _) = true


ls :: Path -> Array Path
ls (Directory _ files) = files
ls _                   = []

size :: Path -> Maybe Int
size (File _ bytes) = Just bytes
size _              = Nothing

root :: Path
root =
    Directory "/"
        [ Directory "/bin/"
            [ File "/bin/cp" 24800
            , File "/bin/ls" 34700
            , File "/bin/mv" 20200
            ]
        , Directory "/etc/"
            [ File "/etc/hosts" 300
            ]
        , Directory "/home/"
            [ Directory "/home/user/"
                [ File "/home/user/todo.txt" 1020
                , Directory "/home/user/code/"
                    [ Directory "/home/user/code/js/"
                        [ File "/home/user/code/js/test.js" 40000
                        ]
                    , Directory "/home/user/code/haskell/"
                        [ File "/home/user/code/haskell/test.hs" 5000
                        ]
                    ]
                ]
            ]
        ]
