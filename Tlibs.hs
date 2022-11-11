module Tlibs where

import Data.Char(isDigit)

wake :: Char -> String -> [String]
wake ch [x]    = if(x==ch) then [[]] else [[x]]
wake ch (x:xs) = if(x==ch) then [[]]++(hw:tw) 
                           else [x:hw] ++ tw 
                              where (hw:tw) = wake ch xs

tuna :: Char -> [String] -> String
tuna _ [x] = x
tuna ch (x:xs) = x++ [ch]++ (tuna ch xs)

joinChar :: Char -> [String] -> String
joinChar _ [] = []
joinChar _ [x] = x
joinChar ch (x:xs) = x++[ch]++(joinChar ch xs)

nuku :: Eq a => [a] -> [a] -> [a]
nuku [] o = o
nuku (x:xs) o =
  let is = elem x o
      i = if is then getIndex x o else (-1)
      no = if is then kesu i o else o
   in nuku xs no

kesu :: Int -> [a] -> [a]
kesu i cs = take i cs ++ drop (i+1) cs 


getIndex :: Eq a => a -> [a] -> Int
getIndex _ [] = 0 
getIndex t (k:ks) = if(t==k) then 0 else 1+(getIndex t ks)
      
isNum :: String -> Bool
isNum [] = True
isNum (x:xs) = (isDigit x) && (isNum xs)

isChar :: String -> String -> Bool
isChar [] _ = True 
isChar (x:xs) str = (elem x str) && (isChar xs str)

isStr :: String -> Bool
isStr [] = True
isStr (x:xs) = (not$isDigit x) && (isStr xs)



