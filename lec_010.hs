import Control.Exception (IOException, try)
import System.IO (IOMode(ReadMode), withFile, hGetChar)

main = do
    putStr "Introduzca el nombre de un fichero: "
    fileName <- getLine
    e <- try(withFile fileName ReadMode (\handle -> do
        char1 <- hGetChar handle
        char2 <- hGetChar handle
        return (char1, char2)))

    case e of
        Left exc -> do
            print (exc :: IOException)
            main
        Right (char1, char2) -> putStrLn [char1, char2]