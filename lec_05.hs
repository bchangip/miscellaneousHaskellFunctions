module TiposExample (module TiposExample) where

import Data.Char

type Entero = Integer
type DeEnteroEnEntero = Entero -> Entero

unoS :: Entero
unoS = 1

sucesor :: DeEnteroEnEntero
sucesor x = x + 1

type ParFlotantes = (Float, Float)

parCeros :: ParFlotantes
parCeros = (0.0,0.0)

type Fecha = (Int, Int, Int)
data DiaSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo --deriving Show

data LetraOEntero = Letra Char | Entero Integer deriving Show

numero :: LetraOEntero
numero = Entero 1

letra :: LetraOEntero
letra = Letra 'a'

extractValueN :: LetraOEntero -> Integer
extractValueN (Entero n) = n
extractValueN _ = error "El argumento debe ser Entero n"

extractValueL :: LetraOEntero -> Char
extractValueL (Letra x) = x
extractValueL _ = error "El argumento debe ser Letra x"

createInstanceN :: Integer -> LetraOEntero
createInstanceN n = Entero n

createInstanceL :: Char -> LetraOEntero
createInstanceL x = Letra x

listaMixta :: [LetraOEntero]
listaMixta = [Letra 'a', Entero 12, Entero 0, Letra 'b']

incLoE :: LetraOEntero -> LetraOEntero
incLoE (Entero n) = Entero . (+1) $ n
incLoE (Letra c) = Letra . chr . (+1) . ord $ c

data Nat = Cero | Suc Nat deriving Show

uno :: Nat
uno = Suc Cero

dos :: Nat
dos = Suc uno

cinco :: Nat
cinco = (Suc (Suc (Suc (Suc (Suc Cero)))))

construir :: Int -> Nat
construir 0 = Cero
construir n = Suc (construir (n-1))

deconstruir :: Nat -> Integer
deconstruir Cero = 0
deconstruir (Suc n) = (+1) . deconstruir $ n

infinitoN :: Nat
infinitoN = Suc infinitoN

indefinidoN :: Nat
indefinidoN = undefined

indefinidoI :: Integer
indefinidoI = undefined          --undefined entra en cualquier tipo

esCero :: Nat -> Bool
esCero Cero = True
esCero _ = False

esPar :: Nat -> Bool
esPar Cero = True
esPar (Suc x) = not (esPar x)

infixl 6 <+>
(<+>) :: Nat -> Nat -> Nat
m <+> Cero = m
m <+> (Suc n) = Suc (m <+> n)

infixl 7 <.>
(<.>) :: Nat -> Nat -> Nat
m <.> Cero = Cero
m <.> (Suc n) = m <.> n <+> m

infixl 8 <^>
(<^>) :: Nat -> Nat -> Nat
m <^> Cero = uno
m <^> (Suc n) = m <.> m <^> n

--Un ejemplo de sobrecarga

type Area = Float
type Volumen = Float
type Lado = Float
type Radio = Float

data Cuadrado = UnCuadrado Lado deriving Show
data Rectangulo = UnRectangulo Lado Lado deriving Show
data Triangulo = UnTriangulo Lado Lado Lado deriving Show
data Circulo = UnCirculo Radio deriving Show
data Cubo = UnCubo Lado deriving Show
data Esfera = UnaEsfera Radio deriving Show

class TieneArea a where
    area :: a -> Area

instance TieneArea Cuadrado where
    area (UnCuadrado l) = l^2

instance TieneArea Rectangulo where
    area (UnRectangulo l a) = l*a

instance TieneArea Circulo where
    area (UnCirculo r) = pi*(r^2)

instance TieneArea Triangulo where
    area (UnTriangulo a b c) = sqrt( s *(s-a) * (s-b) * (s-c) ) where
        s = (a+b+c)/2

--Clases estandar
--Eq
--Por lo menos hay que definir ==
instance Eq Nat where
    Cero    == Cero     = True
    Suc m   == Suc n    = m == n
    _       == _        = False

data Color = Rojo | Amarillo | Azul | Verde deriving Eq

instance Ord Color where
    compare Verde Azul = GT
    compare Azul Amarillo = GT
    compare Amarillo Rojo = GT
    compare Verde Verde = EQ
    compare Azul Azul = EQ
    compare Amarillo Amarillo = EQ
    compare Rojo Rojo = EQ

data Temperatura = Far Float | Cel Float deriving Show --Si se hubiera incluido en Eq automaticamente no se hubieran podido comparar unidades.

convTemp :: Temperatura -> Temperatura
convTemp (Far x) = Cel (((x-32.0) / 9.0)*5.0)
convTemp (Cel x) = Far (((x*9.0)/5)+32.0)

instance Eq Temperatura where
    Far x == Far y = x == y
    Cel x == Cel y = x == y
    x == y = x == convTemp y

{-
instance Ord Temperatura where
    Far x   <= Far y    = x <= y
    Cel x   <= Cel y    = x <= y
    x       <= y        = x <= convTemp y
-}

instance Ord Temperatura where
    compare (Far x) (Far y) = compare x y
    compare (Cel x) (Cel y) = compare x y
    compare x y = compare x (convTemp y)
















