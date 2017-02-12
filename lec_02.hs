module Lec_01 (module Lec_01) where
import Data.List (sort)
import Data.Char

len :: [a] -> Int
len [] = 0
len (_:resto) = 1 + len resto --Este patron siempre toma la cabeza, no hay patron predefinido para el ultimo elemento de una lista, por complejidad computacional

revertirM :: [a] -> [a]
revertirM [] = []
revertirM (cabeza:resto) = (revertirM resto) ++ [cabeza] --Importante colocar el parentesis para que

revertirAux :: [a] -> [a] -> [a]
revertirAux lst [] = lst
revertirAux lst (x:xs) = revertirAux (x:lst) xs

revertir :: [a] -> [a]
revertir = revertirAux []

infixl 9 !!! --La l define asociatividad por la izquierda, y el 9 la prioridad (Puede estar de 0 a 9)
(!!!) :: [a] -> Integer -> a --Extrae el enesimo termino de una lista, se colocan los parentesis para convertirla a prefija para la definicion
(x:_) !!! 0 = x
(_:xs) !!! n | n > 0 = xs !!! (n-1)

--a !!! n | n < 0 = (revertir a) !!! n             Intentar hacer el indice reverso

(_:_) !!! _ = error "Negative index"
[] !!! _ = error "Index too large"


divmodAlt :: Integral a => a -> a -> (a,a)
divmodAlt _ 0 = error "Zero division"
divmodAlt m n = (quot m n, rem m n)

-- divMod incluido en Haskell coincide con divmod implementado aqui
divmod :: Integral a => a -> a -> (a,a)
divmod _ 0 = error "Zero division"
divmod m n = (div m n, mod m n)

descartar :: Int -> [a] -> [a]
descartar _ [] = []
descartar n lst@(head:tail)      --Notar la notacion de la lista, podemos referirnos a ella por su nombre o por patron
    | n <= 0 = lst
    | otherwise = descartar (n-1) tail

tomar :: Int -> [a] -> [a]
tomar m lst = case (m, lst) of
                (0,_) -> []
                (_,[]) -> []
                (n,x:xs) -> f n (x:xs)
                    where
                        f :: Int -> [a] -> [a]
                        f n (x:xs)
                            | n < 0 = []
                            | otherwise = x : tomar (n-1) xs --Va encabezando x en la lista

elemento :: Eq a => a -> [a] -> Bool
elemento _ [] = False
elemento x (y:ys)
    | x == y = True
    | otherwise = elemento x ys


head' :: [a] -> a
head' xs = case xs of
    [] -> error "; head' no funciona con listas vacias"
    (x:_) -> x

describeLista :: [a] -> String
describeLista xs = "The list is " ++ what xs
    where 
        what [] = "empty"
        what [x] = "a singleton list."
        what xs = "a longer list."

describeLst :: [a] -> String       --Alternativa
describeLst xs = "La lista es " ++ case xs of
    [] -> "una lista vacia."
    [x] -> "una lista unitaria."
    _ -> "una lista larga."

maximum' :: Ord a => [a] -> a
maximum' [] = error "Maximo de una lista vacia"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where
        maxTail = maximum' xs


maximum'' :: Ord a => [a] -> a
maximum'' [] = error "Maximo de una lista vacia"
maximum'' [x] = x
maximum'' (x:xs) = x `max` maximum'' xs

generarLista :: Int -> a -> [a]
generarLista n x = [x | i <- [0..(n-1)]]

generarLista' :: Int -> a -> [a]    --Alternativa
generarLista' n x
    | n == 0 = []
    | otherwise = x : generarLista' (n-1) x

{-
    a, b enteros  ->  existen t, s enteros tal que GCD(a,b) = ta+sb

    Caso b=0
        GCD(a,b)
        GCD(a,0) = a = 1*a + 0*0
            t = 1
            s = 0
    Caso b neq 0
        GCD(a,b) 
        = GCD(b, a `mod` b) 
        = t*b + s*(a `mod` b) 
        = t*b + s*(a - (a `div` b)*b)
        = sa + (t-(a `div` b)*s)*b

        GCD(a,b) = (b, a `mod` b)
        t' = s
        s' = t-(a `div` b)*s
        Nota: a = (a `div` b)*b + (a `mod` b)
-}

eGCD :: Int -> Int -> (Int,Int,Int)
eGCD a 0 = (a,1,0)
eGCD a b = (x,z,y-(a `div` b)*z) where
    (x,y,z) = eGCD b (a `mod` b)























