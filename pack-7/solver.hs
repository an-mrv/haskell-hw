import System.IO 
import qualified Data.ByteString as B
import System.Directory

--task1
printFile :: FilePath -> IO ()
printFile fileToPrint = do
    handle <- openFile fileToPrint ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle 

--task2
areEqualText :: FilePath -> FilePath -> IO (Bool)
areEqualText file1 file2 = do 
    contents1 <- readFile file1  
    contents2 <- readFile file2
    return (contents1 == contents2)

--task3
dos2unix :: FilePath -> IO ()
dos2unix file = do
    contents <- readFile file
    putStr (dos2unix' contents) 
    where dos2unix' = foldr (\x acc -> if x == '\r' then acc else x : acc) ""
    
unix2dos :: String -> IO ()
unix2dos file = do
    contents <- readFile file
    putStr (unix2dos' contents) 
    where unix2dos' = foldr (\x acc -> if x == '\n' then '\r':x:acc else x : acc) ""

--task4
areEqualBin :: FilePath -> FilePath -> IO (Bool)
areEqualBin file1 file2 = do
    contents1 <- B.readFile file1
    contents2 <- B.readFile file2
    return (contents1 == contents2)

--task5
fileIsBeingEdited :: FilePath -> IO (Bool)
fileIsBeingEdited s = doesFileExist (s ++ ".sw")
