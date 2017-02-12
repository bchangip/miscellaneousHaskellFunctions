module LaIO (module LaIO) where

import Data.Char (toUpper)
import Control.Exception

ponerEnMayusculas :: IO()
ponerEnMayusculas = do  
    putStr "Introduce a text: "
    xs <- getLine
    putStr "In uppercase is: "
    putStrLn (map toUpper xs)

--Pueden concatenarse acciones en una lista
acciones :: [IO()]
acciones = [putStr "Introduce a name: ", response] where
    response = do
        xs <- getLine
        putStrLn (concat ["Hola ", xs])

muestraFichero :: IO()
muestraFichero = do
    putStr "Introduzca el nombre de un fichero: "
    name <- getLine
    content <- readFile name
    putStrLn content

-- muestraFichero' :: IO()
-- muestraFichero' = muestraFichero `catch` errorHandler where
--     errorHandler :: SomeException -> IO()
--     errorHandler err = do
--         putStrLn (concat ["Se produjo el siguiente error: ", show err])
--         muestraFichero'

muestraFichero' :: IO()
muestraFichero' = muestraFichero `catch` errorHandler where
    errorHandler :: SomeException -> IO()
    errorHandler err = sequence_ [putStrLn (concat ["Se produjo el siguiente error: ", show err]), muestraFichero']

-- leerEntero :: IO Integer
-- leerEntero = readLn `catch` errorHandler where
--     errorHandler :: SomeException -> IO Integer
--     errorHandler err = sequence_ [putStrLn (concat ["Se produjo el siguiente error: ", show err]), leerEntero]

leerEntero :: IO Integer
leerEntero = do
    putStr "Introduce an integer: "
    readLn `catch` errorHandler where
        errorHandler :: SomeException -> IO Integer
        errorHandler err = do
            putStrLn.concat $ ["Se produjo el siguiente error: ", show err]
            leerEntero

leerEntero' :: IO()
leerEntero' = do
    x <- leerEntero
    putStrLn (concat ["The integer introduced was: ", show x])

