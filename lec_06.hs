module Lec_06 (module Lec_06) where

suma :: Num a => [a] -> a
suma [] = 0
suma (x:xs) = (+) x (suma xs)

concatenar :: [[a]] -> [a]
concatenar [] = []
concatenar (xs:xss) = (++) xs (concatenar xss)

{-
fun [] = z
fun (x:xs) = f x (fun xs)
-}

foldr' :: (a -> b -> b) -> b -> ([a] -> b)
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

foldr'' :: (a -> b -> b) -> b -> ([a] -> b)
foldr'' f z = fun where
    --fun :: [a] -> b
    fun [] = z
    fun (x:xs) = f x (fun xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs

all' :: Foldable t => (a -> Bool) -> t a -> Bool
all' f = foldr (\ x xs -> (f x) && xs) True

any' :: Foldable t => (a -> Bool) -> t a -> Bool
any' f = foldr (\ x xs -> (f x) || xs) False

all'' :: Foldable t => (a -> Bool) -> t a -> Bool
all'' f = foldl (\ xs x -> (f x) && xs) True

any'' :: Foldable t => (a -> Bool) -> t a -> Bool
any'' f = foldl (\ xs x -> (f x) || xs) False

