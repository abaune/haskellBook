module Cipher where

import Data.Char

baseValue :: Int
baseValue = ord 'a'

ceaser :: String -> Int -> String
ceaser (x:xs) shiftSize = (chr
                        . (+baseValue)
                        . (`mod` 26)
                        . (`mod` baseValue)
                        . (+shiftSize)
                        . ord) x : ceaser xs shiftSize
ceaser [] _ = []

unCeaser :: String -> Int -> String
unCeaser xs shiftSize = ceaser xs (26 - shiftSize)
