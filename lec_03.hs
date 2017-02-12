module Tipos (module Tipos) where

type Entero = Integer --Unicamente se le da un sinonimo para Integer
type DeEnteroEnEntero = Entero -> Entero

uno :: Entero
uno = 1

sucesor :: DeEnteroEnEntero
sucesor x = x + 1

type ParFlotantes = (Float, Float)

parCeros :: ParFlotantes
parCeros = (0.0,0.0)

-- Por defecto en Prelude: type String = [Char]

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


esFinDe :: DiaSemana -> Bool
esFinDe Sabado = True
esFinDe Domingo = True
esFinDe _ = False

diaSiguiente :: DiaSemana -> DiaSemana
diaSiguiente Lunes = Martes
diaSiguiente Martes = Miercoles
diaSiguiente Miercoles = Jueves
diaSiguiente Jueves = Viernes
diaSiguiente Viernes = Sabado
diaSiguiente Sabado = Domingo
diaSiguiente Domingo = Lunes

data Mes = Enero | Febrero | Marzo | Abril | Mayo | Junio | Julio | Agosto | Septiembre | Octubre | Noviembre | Diciembre deriving Show

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







