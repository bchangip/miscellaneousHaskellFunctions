module Fibonacci (module Fibonacci) where

infixl 9 !!! --La l define asociatividad por la izquierda, y el 9 la prioridad (Puede estar de 0 a 9)
(!!!) :: [a] -> Integer -> a --Extrae el enesimo termino de una lista, se colocan los parentesis para convertirla a prefija para la definicion
(x:_) !!! 0 = x
(_:xs) !!! n | n > 0 = xs !!! (n-1)
--a !!! n | n < 0 = (revertir a) !!! n             Intentar hacer el indice reverso
(_:_) !!! _ = error "Negative index"
[] !!! _ = error "Index too large"


twofib :: Integer -> (Integer,Integer)
twofib 0 = (0,1)
twofib n = (b,a+b) where
    (a,b) = twofib (n-1)

fastfib :: Integer -> Integer
fastfib n = fst.twofib $n

fibv0Aux :: Integer -> Integer -> [Integer]
fibv0Aux a b = a:fibv0Aux b (a+b)

fibv0 :: Integer -> Integer
fibv0 n = (fibv0Aux 0 1) !!! n

fibv1 :: [Integer]
-- fibv1 = 0 : 1 : [a+b | (a,b) <- zip fibv1 (tail fibv1)]
fibv1 = 0 : 1 : zipWith (+) fibv1 (tail fibv1)

fibs :: [Integer]
fibs@(0:tfibs) = 0 : 1 : zipWith (+) fibs tfibs

fibsgen :: Integer -> Integer -> [Integer]
fibsgen a b = a : b : zipWith (+) (fibsgen a b) (tail (fibsgen a b))

reverseGen :: [a] -> [a] -> [a]
reverseGen [] mst = mst
reverseGen (x:xs) mst = reverseGen xs (x:mst)

pascal :: [[Integer]]
pascal = [1] : map f pascal where
    f cs = zipWith (+) (0:cs) (reverseGen cs [0])

combinatorialR :: Integer -> Integer -> Integer
combinatorialR m n = (pascal !!! m) !!! b where
    b = min m (m-n)