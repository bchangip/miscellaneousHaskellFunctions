--Tarea 1 - Bryan Chan - Pasaporte 300216483

module Tarea1 (module Tarea1) where

--Congruencia de Zeller--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejemplo de uso: zeller (dd,mm,aaaa)
type Fecha = (Int, Int, Int)
data DiaSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo --deriving Show

instance Show DiaSemana where
    showsPrec _ Lunes = showChar 'L'
    showsPrec _ Martes = showChar 'M'
    showsPrec _ Miercoles = showChar 'X'
    showsPrec _ Jueves = showChar 'J'
    showsPrec _ Viernes = showChar 'V'
    showsPrec _ Sabado = showChar 'S'
    showsPrec _ Domingo = showChar 'D'

mesACodigo :: Int -> Int
mesACodigo 1 = 13
mesACodigo 2 = 14
mesACodigo 3 = 3
mesACodigo 4 = 4
mesACodigo 5 = 5
mesACodigo 6 = 6
mesACodigo 7 = 7
mesACodigo 8 = 8
mesACodigo 9 = 9
mesACodigo 10 = 10
mesACodigo 11 = 11
mesACodigo 12 = 12

firstTwoDigits :: Int -> Int
firstTwoDigits a = a `div` 100

lastTwoDigits :: Int -> Int
lastTwoDigits a = a - (firstTwoDigits a)*100

idToDia :: Int -> DiaSemana
idToDia 0 = Sabado
idToDia 1 = Domingo
idToDia 2 = Lunes
idToDia 3 = Martes
idToDia 4 = Miercoles
idToDia 5 = Jueves
idToDia 6 = Viernes

zellerAux :: Fecha -> Int
zellerAux (dia,mes,ano)
    | mes == 1 = (dia + (13*((mesACodigo mes)+1) `div` 5) + (lastTwoDigits (ano-1)) + ((lastTwoDigits (ano-1)) `div` 4) + ((firstTwoDigits (ano-1)) `div` 4) - (2*(firstTwoDigits (ano-1)))) `mod` 7
    | mes == 2 = (dia + (13*((mesACodigo mes)+1) `div` 5) + (lastTwoDigits (ano-1)) + ((lastTwoDigits (ano-1)) `div` 4) + ((firstTwoDigits (ano-1)) `div` 4) - (2*(firstTwoDigits (ano-1)))) `mod` 7
    | otherwise = (dia + (13*((mesACodigo mes)+1) `div` 5) + (lastTwoDigits (ano)) + ((lastTwoDigits (ano)) `div` 4) + ((firstTwoDigits (ano)) `div` 4) - (2*(firstTwoDigits (ano)))) `mod` 7

zeller :: Fecha -> DiaSemana
zeller (dia,mes,ano) = idToDia(zellerAux (dia,mes,ano))
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-
Sean M, N, M', N' lambda-terminos.
    1.  Si M=M' entonces M[x:=N] = M'[x:=N]
    2.  Si N=N' entonces M[x:=N] = M[x:=N']     //Demostracion por induccion
    3.  Si M=M' y N=N' entonces M[x:=N]=M'[x:=N']

    Demostracion 1:
        Supongamos M=M' y consideremos la regla chi, por lo que lambdax.M = lambdax.M' y por regla 6
        (lambdax.M)N = (lambdax.M')N y por beta-conversion
        M[x:=N] = M'[x:=N]

    Demostracion 2:
        

    Demostracion 3:
        M = M'
        M[x:=N] = M'[x:=N]  //Por inciso 1
        M[x:=N] = M'[x:=N'] //Por inciso 2
        M[x:=N] = M'[x:=N'] //Por transitividad de la igualdad
-}

--Combinatoria------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejemplo de uso: combinatorialR a b
infixl 9 !!! --La l define asociatividad por la izquierda, y el 9 la prioridad (Puede estar de 0 a 9)
(!!!) :: [a] -> Integer -> a --Extrae el enesimo termino de una lista, se colocan los parentesis para convertirla a prefija para la definicion
(x:_) !!! 0 = x
(_:xs) !!! n | n > 0 = xs !!! (n-1)
(_:_) !!! _ = error "Negative index"
[] !!! _ = error "Index too large"

reverseGen :: [a] -> [a] -> [a]
reverseGen [] mst = mst
reverseGen (x:xs) mst = reverseGen xs (x:mst)

pascal :: [[Integer]]
pascal = [1] : map f pascal where
    f cs = zipWith (+) (0:cs) (reverseGen cs [0])

combinatorialR :: Integer -> Integer -> Integer
combinatorialR m n = (pascal !!! m) !!! b where
    b = min m (m-n)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Bezout------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejemplo de uso: eGCD a b
eGCD :: Int -> Int -> (Int,Int,Int)
eGCD a 0 = (a,1,0)
eGCD a b = (x,z,y-(a `div` b)*z) where
    (x,y,z) = eGCD b (a `mod` b)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


--Congruencias------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Ejemplo de uso: solveCongruenceSystem [Congruence a1 b1 m1, Congruence a2 b2 m2, ..., Congruence an bn mn]
class Normalizable a where
    norm :: a -> a

data Congruence a = Congruence {
    a :: Int,
    b :: Int,
    m :: Int
}

instance Show a => Show (Congruence a) where
    showsPrec _ (Congruence a b m) = shows a . showString "x ≣ " . shows b . showString " (mod " . shows m . showString ")"

instance Normalizable (Congruence a) where
    norm (Congruence a b m)
        | m == 0 = error "mod 0 is undefined."
        | b `mod` gcdResult /= 0 = error "A congruence given doesnt have a solution."
        | m < 0 = norm (Congruence a b (abs m))
        | otherwise = Congruence 1 b' m'
        where
            (gcdResult, aCoefficient, mCoefficient) = eGCD a m
            m' = (m `div` gcdResult)
            b' = ((b `div` gcdResult) * aCoefficient) `mod` m'

solvePairCongruences :: Congruence a -> Congruence a -> Congruence a
solvePairCongruences (Congruence a1 b1 m1) (Congruence a2 b2 m2)
    | ((b2' - b1') `mod` gcdResult) /= 0 = error "A pair of congruences are not compatible"
    | otherwise = (Congruence 1 bResult mResult) 
        where
            (Congruence a1' b1' m1') = norm (Congruence a1 b1 m1)
            (Congruence a2' b2' m2') = norm (Congruence a2 b2 m2)
            (gcdResult, m1Coefficient, m2Coefficient) = eGCD m1' m2'
            s = m1 `div` gcdResult
            c = (b2'-b1') `div` gcdResult
            m = m2' `div` gcdResult
            (Congruence a3' b3' m3') = norm (Congruence s c m)
            (gcdResult2, sCoefficient, mCoefficient) = eGCD s m
            u' = (sCoefficient*c) `mod` m

            mResult = m1'*m
            bResult = (b1' + m1'*u')

solveCongruenceSystem :: [Congruence a] -> Congruence a
solveCongruenceSystem x = foldr solvePairCongruences (Congruence 1 0 1) x
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--Sieve of Eratosthenes
sievePrimesAux :: [Int] -> [Int]
sievePrimesAux [] = []
sievePrimesAux (x:xs) = x:(sievePrimesAux possiblePrimes) where
    possiblePrimes = filter (\y -> (y `mod` x) /= 0) xs

sievePrimes :: Int -> [Int]
sievePrimes upperLimit = sievePrimesAux [2..upperLimit]