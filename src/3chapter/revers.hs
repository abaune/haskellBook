module Reverse where

rvrs :: String -> String
rvrs str = end ++ " " ++ mid ++ " " ++ beg
  where beg = take 5 str
        mid = ((take 2) . (drop 6)) str
        end = drop 9 str

main :: IO ()
main = print $ rvrs "Curry is awesome"
