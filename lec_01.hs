module Aritmetica (module Aritmetica) where

import Data.Char
import Data.List
import Data.Maybe

alfabeto :: String
alfabeto = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567.,:;"

indexToChar' :: Int -> Char
indexToChar' n
    | n `elem` [0..39] = alfabeto !! n
    | otherwise = error "Indice inapropiado"

indexToChar :: Int -> Maybe Char
indexToChar n
    | n `elem` [0..39] = Just(alfabeto !! n)
    | otherwise = Nothing

charToIndex :: Char -> Int
charToIndex c = fromJust(elemIndex c alfabeto)


ebAux :: Int -> Int -> [Int] -> [Int]
ebAux 0 _ xs = xs
ebAux n b xs = ebAux (n `div` b) b ((n `mod` b):xs)

eb :: Int -> Int -> [Int]
eb n m = ebAux n m []

te :: Int -> String
te 0 = ""
te n = te((n+1) `div` 3) ++ ["0+-"!!(n `mod` 3)]

