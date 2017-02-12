module Lec_07 (module Lec_07) where

data Arbol a = Vacio 
            | Nodo a [Arbol a] 
            deriving Show

data ArbolB a = VacioB
                | NodoB (ArbolB a) a (ArbolB a)
                deriving Show

hoja :: a -> Arbol a
hoja x = Nodo x []

hojaB :: a -> ArbolB a
hojaB x = NodoB VacioB x VacioB

a1 :: Arbol Int
a1 = Nodo 10 [a11, a12, a13] where
    a11 = Nodo 22 [hoja 15, hoja 12]
    a12 = hoja 35
    a13 = Nodo 52 [hoja 33]

a2 :: ArbolB Int
a2 = NodoB aI 10 aD where
    aI = NodoB aII 15 aID
    aD = NodoB aDI 18 aDD
    aII = hojaB 24
    aID = hojaB 27
    aDI = VacioB
    aDD = hojaB 24

raiz :: Arbol a -> a
raiz Vacio = error "Raiz de arbol vacio"
raiz (Nodo n _) = n

tamano :: Arbol a -> Int
tamano Vacio = 0
tamano (Nodo n xs) = (+1).sum.map tamano $ xs

profundidad :: Arbol a -> Int
profundidad Vacio = 0
profundidad (Nodo _ []) = 1
profundidad (Nodo _ xs) = (+1).maximum.map profundidad $ xs

nivel :: Int -> Arbol a -> [a]
nivel i Vacio = error (concat["An empty tree doesnt have the level ",show i])
nivel 0 (Nodo x xs) = [x]
nivel i (Nodo x []) = []
nivel i (Nodo x xs) = concat [nivel (i-1) j | j <- xs]

raizB :: ArbolB a -> a
raizB VacioB = error "Raiz del arbol binario vacio"
raizB (NodoB _ n _) = n

tamanoB :: ArbolB a -> Int
tamanoB VacioB = 0
tamanoB (NodoB i r d) = (+1).sum.map tamanoB $ [i,d]

instance Functor ArbolB where
    fmap f VacioB = VacioB
    fmap f (NodoB i r d) = (NodoB (fmap f i) (f r) (fmap f d))

instance Functor Arbol where
    fmap f Vacio = Vacio
    fmap f (Nodo x xs) = (Nodo (f x) (map (fmap f) xs))

duplicar :: (Num a, Functor f) => f a -> f a
duplicar = fmap (*2)

data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
            deriving Show

instance Foldable Tree where
    foldr f z Empty = z
    foldr f z (Leaf x) = f x z
    foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l

sumarTree :: Num a => Tree a -> a
sumarTree = foldr (\ x y -> (x+y)) 0

a3 :: Tree Int
a3 = Node aI 10 aD where
    aI = Node aII 15 aID
    aD = Node aDI 18 aDD
    aII = Leaf 24
    aID = Leaf 27
    aDI = Empty
    aDD = Leaf 24

plegarPreOrden :: Tree a -> [a]
plegarPreOrden = foldr (\ y ys -> y : ys) []

plegarPreOrdenl :: Tree a -> [a]
plegarPreOrdenl = foldl (\ ys y -> y : ys) []

plegarEnOrden :: Tree a -> [a]
plegarEnOrden Empty = []
plegarEnOrden (Leaf x) = [x]
plegarEnOrden (Node l c r) = concat [[c], plegarEnOrden l, plegarEnOrden r]










